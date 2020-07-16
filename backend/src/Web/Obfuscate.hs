{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Obfuscate where

import Data.Tagged
import Data.Kind
import Data.List (nub)
import Control.Monad 
import Text.Read (readMaybe)
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat, natVal, KnownNat, KnownSymbol, Symbol)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Maybe as Maybe
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))

import qualified Hashids as H
import Web.HttpApiData
import Servant
import Servant.Server.Internal
import Servant.API.ContentTypes
import Network.HTTP.Types (hContentType, hAccept, Method, Status, HeaderName)
import Network.Wai (lazyRequestBody,responseLBS, requestHeaders)
import Database.Persist
import Database.Persist.Sql
import Data.Aeson 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Scientific (isInteger, toBoundedInteger)
import Data.HashMap.Strict (mapWithKey)
import qualified Data.HashMap.Strict as HashMap
import qualified Language.Haskell.TH as TH

data ObfuscationOptions = ObfuscationOptions
  { obfuscatedFieldPrefix :: String
  , obfuscatedConstructorPrefix :: String
  }

defaultObfuscationOptions :: ObfuscationOptions
defaultObfuscationOptions = ObfuscationOptions
  { obfuscatedFieldPrefix = "ob"
  , obfuscatedConstructorPrefix = "Obfuscated"
  }

deriveObfuscate :: ObfuscationOptions-> TH.Name -> TH.Q [TH.Dec]
deriveObfuscate options tyName = do
  TH.TyConI ty <- TH.reify tyName
  (obfuscatedName, obfuscatedTy) <- mkObfuscatedType ty 
  let obfuscatedInstance = TH.TySynInstD ''Obfuscated (TH.TySynEqn [TH.ConT tyName] (TH.ConT obfuscatedName))
  canObfuscate <- generateCanObfuscate ty
  canDeobfuscate <- generateCanDeobfuscate ty
  let canObfuscateInstance = TH.InstanceD Nothing [] (TH.ConT ''CanObfuscate `TH.AppT` TH.ConT tyName) [canObfuscate]
  let canDeobfuscateInstance = TH.InstanceD Nothing [] (TH.ConT ''CanDeobfuscate `TH.AppT` TH.ConT tyName) [canDeobfuscate]
  pure [obfuscatedTy, obfuscatedInstance, canObfuscateInstance, canDeobfuscateInstance]
    where 
      fieldPrefix = obfuscatedFieldPrefix options
      constructorPrefix = obfuscatedConstructorPrefix options

      obfuscatedFieldName n = TH.mkName (fieldPrefix ++ TH.nameBase n) 
      obfuscatedConstructorName n = TH.mkName (constructorPrefix ++ TH.nameBase n) 

      generateCanObfuscate (TH.DataD _ _ _ _ cs _) = do 
        clauses <- traverse generateObfuscateClause cs
        pure $ TH.FunD (TH.mkName "obfuscate") clauses
      generateCanObfuscate (TH.NewtypeD _ _ _ _ c _) = do
        clauses <- traverse generateObfuscateClause [c] 
        pure $ TH.FunD (TH.mkName "obfuscate") clauses

      generateCanDeobfuscate (TH.DataD _ _ _ _ cs _) = do
        clauses <- traverse generateDeobfuscateClause cs
        pure $ TH.FunD (TH.mkName "deobfuscate") clauses
      generateCanDeobfuscate (TH.NewtypeD _ _ _ _ c _) = do
        clauses <- traverse generateDeobfuscateClause [c]
        pure $ TH.FunD (TH.mkName "deobfuscate") clauses

      generateDeobfuscateClause (TH.NormalC n tys) = do
        let ctx = TH.mkName "ctx"
            x = TH.mkName "x"
        body <- [| undefined |]
        pure $ TH.Clause [TH.VarP ctx, TH.VarP x] (TH.NormalB body) [] 

      generateDeobfuscateClause (TH.RecC n tys) = do
        let ctx = TH.mkName "ctx"
            x = TH.mkName "x"
            pureFn = TH.VarE $ TH.mkName "pure"
        (deobfuscatedBinds, deobfuscatedFields) <- fmap mconcat $ traverse assignDeobfuscatedField tys

        let body = TH.DoE $ join 
                    [ fmap (\(p, e) -> TH.BindS p e) deobfuscatedBinds
                    , [TH.NoBindS (pureFn `TH.AppE` (TH.RecConE n deobfuscatedFields))]
                    ]
        pure $ TH.Clause [TH.VarP ctx, TH.VarP x] (TH.NormalB body) [] 

      assignDeobfuscatedField (n, _, ty) = do
        isObfuscateable <- isFieldObfuscateable ty 
        if isObfuscateable then do
          let deobfuscatedFieldName = TH.mkName $ "d" ++ TH.nameBase n
          deobfuscatedE <- [| deobfuscate ctx $ $(TH.varE $ obfuscatedFieldName n) x |]
          pure $ ([(TH.VarP deobfuscatedFieldName, deobfuscatedE)], [(n, TH.VarE deobfuscatedFieldName)])
        else do
          fieldValue <- [|$(TH.varE $ obfuscatedFieldName n) x|]
          pure $ ([], [(n, fieldValue)])

      obfuscateNormalField (x, (_, ty)) = do
        isObfuscateable <- isFieldObfuscateable ty 
        if isObfuscateable then
          [| obfuscate ctx $(TH.varE x) |]
        else 
          TH.varE x

      generateObfuscateClause c@(TH.NormalC n tys) = do
        let ctx = TH.mkName "ctx"
        xs <- traverse (const $ TH.newName "x") tys
        let ps = fmap TH.VarP xs
        fields <- traverse obfuscateNormalField (zip xs tys) 
        let body = foldl TH.AppE (TH.ConE (obfuscatedConstructorName n)) fields
        pure $ TH.Clause [TH.VarP ctx, TH.ConP n ps] (TH.NormalB body) [] 
      
      generateObfuscateClause (TH.RecC n tys) = do
        let ctx = TH.mkName "ctx"
            x = TH.mkName "x"
        obfuscatedFields <- traverse assignObfuscatedField tys
        let body = TH.RecConE (obfuscatedConstructorName n) obfuscatedFields
        pure $ TH.Clause [TH.VarP ctx, TH.VarP x] (TH.NormalB body) [] 

      assignObfuscatedField (fieldName, _, ty) = do
        isObfuscateable <- isFieldObfuscateable ty 
        (,) <$> pure (obfuscatedFieldName fieldName) <*>
          if isObfuscateable then
            [|obfuscate ctx ($(TH.varE fieldName) x)|]
          else 
            [|$(TH.varE fieldName) x|]

      mkObfuscatedType (TH.DataD cxt n tvbs mKind cs dcs) = do
        obfuscatedCs <- traverse obfuscateConstructor cs 
        let obfuscatedName = obfuscatedConstructorName n
        pure $ (obfuscatedName, TH.DataD cxt obfuscatedName tvbs mKind obfuscatedCs dcs)
      mkObfuscatedType (TH.NewtypeD cxt n tvbs mKind c dcs) = do
        obfuscatedC <- obfuscateConstructor c
        let obfuscatedName = obfuscatedConstructorName n
        pure $ (obfuscatedName, TH.NewtypeD cxt obfuscatedName tvbs mKind obfuscatedC dcs)

      obfuscateConstructor (TH.NormalC n tys) = do
        obfuscatedTys <- traverse (\(b, t) -> (,) <$> pure b <*> obfuscateField t) tys
        let obfuscatedName = obfuscatedConstructorName n
        pure $ TH.NormalC obfuscatedName obfuscatedTys
      obfuscateConstructor (TH.RecC n tys) = do
        obfuscatedTys <- traverse (\(n, b, t) -> (,,) <$> pure (obfuscatedFieldName n) <*> pure b <*> obfuscateField t) tys
        let obfuscatedName = obfuscatedConstructorName n
        pure $ TH.RecC obfuscatedName obfuscatedTys
      obfuscateConstructor (TH.GadtC ns tys ty) = do
        obfuscatedTys <- traverse (\(b, t) -> (,) <$> pure b <*> obfuscateField t) tys
        let obfuscatedNames = fmap obfuscatedConstructorName ns
        pure $ TH.GadtC obfuscatedNames obfuscatedTys ty
      obfuscateConstructor (TH.RecGadtC ns tys ty) = do
        obfuscatedTys <- traverse (\(n, b, t) -> (,,) <$> pure (obfuscatedFieldName n) <*> pure b <*> obfuscateField t) tys
        let obfuscatedNames = fmap obfuscatedConstructorName ns
        pure $ TH.RecGadtC obfuscatedNames obfuscatedTys ty
      obfuscateConstructor (TH.ForallC tvbs cxt c) = do
        TH.ForallC tvbs cxt <$> obfuscateConstructor c
      obfuscateConstructor c = pure c

      -- higher kinded types are only obfuscateable if they wrap an obfuscateable type 
      -- this assumes all higher kinded types are Functor-like 
      isFieldObfuscateable (TH.AppT TH.ListT ty) = isFieldObfuscateable ty
      isFieldObfuscateable ty@(TH.AppT (TH.ConT _) baseTy) = do
        (&&) <$> (fmap (\instances -> length instances > 0) (TH.reifyInstances ''Obfuscated [ty]))
             <*> isFieldObfuscateable baseTy
      isFieldObfuscateable ty = do
        -- Strings are silly, they get a list instance but show up as a ConT
        if ty == TH.ConT ''String then do
           pure False
        else do
          instances <- TH.reifyInstances ''Obfuscated [ty]
          pure $ length instances > 0

      obfuscateField ty = do
        isObfuscateable <- isFieldObfuscateable ty 
        if isObfuscateable then
          pure $ TH.ConT ''Obfuscated `TH.AppT` ty
        else 
          pure ty

type family Obfuscated a 
class CanObfuscate a where
  obfuscate :: H.HashidsContext -> a -> Obfuscated a
class CanDeobfuscate a where
  deobfuscate :: H.HashidsContext -> Obfuscated a -> Maybe a

type instance Obfuscated [a] = [Obfuscated a] 
instance CanObfuscate a => CanObfuscate [a] where
  obfuscate ctx xs = fmap (obfuscate ctx) xs
instance CanDeobfuscate a => CanDeobfuscate [a] where
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

type instance Obfuscated (Maybe a) = Maybe (Obfuscated a)
instance CanObfuscate a => CanObfuscate (Maybe a) where
  obfuscate ctx xs = fmap (obfuscate ctx) xs
instance CanDeobfuscate a => CanDeobfuscate (Maybe a) where
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

obfuscateIntegral :: (Integral a) => H.HashidsContext -> a -> T.Text
obfuscateIntegral ctx i = T.pack $ H.encode ctx [fromIntegral i]

deobfuscateIntegral :: (Read a, Integral a) => H.HashidsContext -> T.Text -> Maybe a
deobfuscateIntegral ctx r = fromIntegral <$>
  (Maybe.listToMaybe $ H.decode ctx $ T.unpack r) <|> (readMaybe $ T.unpack r)

-- A general newtype wrapper for integrals that can be obfuscated
-- make a type alias for your particular use. Or use it as an example. 
newtype ObIntegral a = ObIntegral a
  deriving newtype (Show, Read, Num, Eq, Ord, Enum, Real, Integral)
type instance (Obfuscated (ObIntegral a)) = T.Text
instance Integral a => CanObfuscate (ObIntegral a) where
  obfuscate = obfuscateIntegral
instance (Read a, Integral a) => CanDeobfuscate (ObIntegral a) where
  deobfuscate = deobfuscateIntegral

type instance Obfuscated (Key e) = T.Text
instance (ToBackendKey SqlBackend e) => CanObfuscate (Key e) where
  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral $ fromSqlKey i]
instance (ToBackendKey SqlBackend e) => CanDeobfuscate (Key e) where
  deobfuscate ctx r = toSqlKey <$> deobfuscateIntegral ctx r

class HasObfuscatedServerImplementation api context where
  routeImpl :: Proxy api -> Context context -> Delayed env (Server api) -> Router env 
  hoistServerWithContextImpl :: Proxy api -> Proxy context -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n 

instance ( CanDeobfuscate a
         , Obfuscated a ~ T.Text
         , HasContextEntry context H.HashidsContext
         , KnownSymbol capture
         , HasServer api context
         ) => HasObfuscatedServerImplementation (Capture (capture :: Symbol) a :> api) context where

  hoistServerWithContextImpl _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  routeImpl Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case deobfuscate hashidsContext txt of
                  Just v -> return v 
                  Nothing -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
            )

instance ( CanDeobfuscate a
         , HasContextEntry context H.HashidsContext 
         , AllMimeUnrender ctypes (Obfuscated a)
         , HasServer api context
         ) => HasObfuscatedServerImplementation (ReqBody ctypes a :> api) context where
  hoistServerWithContextImpl _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  routeImpl Proxy context subserver = route (Proxy :: Proxy api) context $
          addBodyCheck subserver ctCheck bodyCheck
    where
      -- Content-Type check, we only lookup we can try to parse the request body
      ctCheck = withRequest $ \ request -> do
        -- See HTTP RFC 2616, section 7.2.1
        -- http://www.w3.org/Protocols/rfc2616/rfc2616-sec7.html#sec7.2.1
        -- See also "W3C Internet Media Type registration, consistency of use"
        -- http://www.w3.org/2001/tag/2002/0129-mime
        let contentTypeH = Maybe.fromMaybe "application/octet-stream"
                         $ lookup hContentType $ requestHeaders request
        case canHandleCTypeH (Proxy :: Proxy ctypes) (cs contentTypeH) of
          Nothing -> delayedFail err415
          Just f  -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \ request -> do
        mrqbody <- f <$> liftIO (lazyRequestBody request)
        case deobfuscate (getContextEntry context) <$> mrqbody of
            Left e         -> delayedFailFatal err400 { errBody = cs e }
            Right Nothing  -> delayedFailFatal err400 
            Right (Just v) -> return v

instance {-# OVERLAPPABLE #-}
         ( ReflectMethod method, KnownNat status
         , HasContextEntry context H.HashidsContext
         , CanObfuscate a
         , AllCTRender ctypes (Obfuscated a)
         ) => HasObfuscatedServerImplementation (Verb (method :: StdMethod) (status :: Nat) (ctypes :: [*]) a) context where
  hoistServerWithContextImpl _ _ nt s = nt s

  routeImpl Proxy ctx = 
    methodRouter (\x -> ([], obfuscate (getContextEntry ctx) x)) method (Proxy :: Proxy ctypes) status
        where method = reflectMethod (Proxy :: Proxy method)
              status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance ( ReflectMethod method, KnownNat status
         , HasContextEntry context H.HashidsContext
         , CanObfuscate a
         , AllCTRender ctypes (Obfuscated a)
         , GetHeaders (Headers ls a)
         ) => HasObfuscatedServerImplementation (Verb (method :: StdMethod) (status :: Nat) (ctypes :: [*]) (Headers ls a)) context where
  hoistServerWithContextImpl _ _ nt s = nt s

  routeImpl Proxy ctx = 
    methodRouter (\x -> (getHeaders x, obfuscate (getContextEntry ctx) (getResponse x))) method (Proxy :: Proxy ctypes) status
        where method = reflectMethod (Proxy :: Proxy method)
              status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

instance {-# OVERLAPPING #-} 
         ( HasObfuscatedServerImplementation api context 
         ) => HasServer (Obfuscate :> api) context where
  type ServerT (Obfuscate :> api) m = ServerT api m 
  hoistServerWithContext _ = hoistServerWithContextImpl (Proxy :: Proxy api)
  route _ = routeImpl (Proxy :: Proxy api)

data Obfuscate
  deriving Typeable

decodeFields :: FromJSON a => H.HashidsContext -> Value -> Result a
decodeFields ctx = fromJSON . go
  where
  go (Object o) = Object $ fmap go o
  go val@(Array a) = Array $ fmap go a
  go val@(String s) =
    Maybe.fromMaybe val $ do 
      i <- Maybe.listToMaybe $ H.decode ctx (cs s)
      pure $ toJSON i
  go val = val
