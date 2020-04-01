{-# LANGUAGE BangPatterns #-}

module Web.Obfuscate where

import Data.Kind
import Text.Read (readMaybe)
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat, natVal, KnownNat, KnownSymbol, Symbol)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Maybe as Maybe
import Data.Int (Int64)
import Control.Monad.IO.Class (liftIO)

import qualified Hashids as H
import Web.HttpApiData
import Servant
import Servant.Server.Internal
import Servant.API.ContentTypes
import Network.HTTP.Types (hContentType, hAccept, Method, Status, HeaderName)
import Network.Wai (lazyRequestBody,responseLBS, requestHeaders)
import Database.Persist
import Database.Persist.Sql
import Data.Aeson (fromJSON, Result(..), Value(..), ToJSON(..), FromJSON(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Scientific (isInteger, toBoundedInteger)
import Data.HashMap.Strict (mapWithKey)

data ObfuscatedCapture (sym :: Symbol) (a :: Type) 
  deriving (Typeable)

instance {-# OVERLAPPABLE #-} 
      (HasContextEntry context H.HashidsContext, KnownSymbol capture, HasServer api context, Integral a)
      => HasServer (ObfuscatedCapture capture a :> api) context where

  type ServerT (ObfuscatedCapture capture a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case (H.decode hashidsContext (T.unpack txt), readMaybe (T.unpack txt)) of
                  (v':_, _) -> return (fromIntegral v')
                  ([], Just v') -> return (fromIntegral v')
                  ([], Nothing) -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
            )

instance (HasContextEntry context H.HashidsContext, KnownSymbol capture, HasServer api context, ToBackendKey SqlBackend e)
      => HasServer (ObfuscatedCapture capture (Key e) :> api) context where

  type ServerT (ObfuscatedCapture capture (Key e) :> api) m = (Key e) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case (H.decode hashidsContext (T.unpack txt), readMaybe (T.unpack txt)) of
                  (v':_, _) -> return (toSqlKey $ fromIntegral v')
                  ([], Nothing) -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
                  ([], Just v') -> return (toSqlKey $ fromIntegral v')
            )

data ObfuscatedReqBody (contentType :: [Type]) (a :: Type)
  deriving (Typeable)

instance (ctypes ~ '[JSON], FromJSON a, HasContextEntry context H.HashidsContext, HasServer api context) 
       => HasServer (ObfuscatedReqBody ctypes a :> api) context where
  type ServerT (ObfuscatedReqBody ctypes a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver
      = route (Proxy :: Proxy api) context $
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
        case canHandleCTypeH (Proxy :: Proxy '[JSON]) (cs contentTypeH) :: Maybe (BL.ByteString -> Either String Value) of
          Nothing -> delayedFail err415
          Just f  -> return f

      -- Body check, we get a body parsing functions as the first argument.
      bodyCheck f = withRequest $ \ request -> do
        mrqbody <- f <$> liftIO (lazyRequestBody request)
        case decodeIdFields (getContextEntry context) <$> mrqbody of
            Left e            -> delayedFailFatal err400 { errBody = cs e }
            Right (Error e)   -> delayedFailFatal err400 { errBody = cs e }
            Right (Success v) -> return v

type ObfuscatedGet   = ObfuscatedVerb GET   200
type ObfuscatedPost  = ObfuscatedVerb POST  200
type ObfuscatedPut   = ObfuscatedVerb PUT   200
type ObfuscatedPatch = ObfuscatedVerb PATCH 200

data ObfuscatedVerb (method :: StdMethod) (status :: Nat) (ctypes :: [Type]) (a :: Type)
  deriving (Typeable)

instance {-# OVERLAPPABLE #-}
         ( ToJSON a, HasContextEntry context H.HashidsContext, ReflectMethod method, KnownNat status
         ) => HasServer (ObfuscatedVerb method status '[JSON] a) context where

  type ServerT (ObfuscatedVerb method status '[JSON] a) m = m a
  hoistServerWithContext _ _ nt s = nt s

  route Proxy context = obfuscatedMethodRouter (getContextEntry context) ((,)[]) method (Proxy :: Proxy '[JSON]) status
      where method = reflectMethod (Proxy :: Proxy method)
            status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

obfuscatedMethodRouter :: (ToJSON a) 
                       => H.HashidsContext
                       -> (b -> ([(HeaderName, B.ByteString)], a))
                       -> Method -> Proxy '[JSON] -> Status
                       -> Delayed env (Handler b)
                       -> Router env
obfuscatedMethodRouter hashidsContext splitHeaders method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH = Maybe.fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
               let (headers, b) = splitHeaders output
               case handleAcceptH proxy (AcceptHeader accH) (encodeIdFields hashidsContext b) of
                 Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
                 Just (contentT, body) ->
                      let bdy = if allowedMethodHead method request then "" else body
                      in Route $ responseLBS status ((hContentType, cs contentT) : headers) bdy


decodeIdFields :: FromJSON a => H.HashidsContext -> Value -> Result a
decodeIdFields ctx = fromJSON . go
  where
  go (Object oMap) = Object $ flip mapWithKey oMap $ \fieldName fieldValue -> 
      case fieldValue of
        v@(Object _) -> 
          go v
        v@(Array a) -> 
          if "ids" `T.isSuffixOf` fieldName then 
            go v
          else 
            v
        v@(String s) -> 
          if "id" `T.isSuffixOf` fieldName then 
            go v
          else 
            v
        v -> v

  go val@(Array a) = Array $ fmap go a

  go val@(String s) =
    Maybe.fromMaybe val $ do 
      i <- Maybe.listToMaybe $ H.decode ctx (cs s)
      pure $ toJSON i

  go val = val

encodeIdFields :: ToJSON a => H.HashidsContext -> a -> Value
encodeIdFields ctx = (go False) . toJSON
  where
  go _ (Object oMap) = Object $ flip mapWithKey oMap $ \fieldName fieldValue -> 
      case fieldValue of
        v@(Object _) -> 
          go False v
        v@(Array a) -> 
          go ("ids" `T.isSuffixOf` fieldName) v
        v@(Number n) -> 
          if "id" `T.isSuffixOf` fieldName then 
            go False v
          else 
            v
        v -> v

  go skip val@(Array a) = Array $ fmap (go skip) a

  go skip val@(Number n) 
    | skip = val
    | not $ isInteger n = val
    | otherwise = res
    where
      res = 
        Maybe.fromMaybe val $ do 
          i <- toInteger <$> toBoundedInteger @Int64 n
          let encoded = H.encode ctx [i]
          pure $ String $ cs encoded

  go _ val = val

