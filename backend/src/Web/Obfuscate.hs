{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}

module Web.Obfuscate where

import Data.Tagged
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
import Data.Aeson 
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Scientific (isInteger, toBoundedInteger)
import Data.HashMap.Strict (mapWithKey)
import qualified Data.HashMap.Strict as HashMap

class Obfuscateable a where
  type Obfuscated a
  obfuscate :: H.HashidsContext -> a -> Obfuscated a
  deobfuscate :: H.HashidsContext -> Obfuscated a -> Maybe a

instance Obfuscateable a => Obfuscateable [a] where
  type Obfuscated [a] = [Obfuscated a] 
  obfuscate ctx xs = fmap (obfuscate ctx) xs
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

instance Obfuscateable Integer where
  type Obfuscated Integer = T.Text
  obfuscate ctx i = T.pack $ H.encode ctx [i]
  deobfuscate ctx r = Maybe.listToMaybe $ H.decode ctx $ T.unpack r

instance Obfuscateable Int where
  type Obfuscated Int = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral i]
  deobfuscate ctx r = fromIntegral <$> (Maybe.listToMaybe $ H.decode ctx $ T.unpack r)

instance Obfuscateable Int64 where
  type Obfuscated Int64 = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral i]
  deobfuscate ctx r = fromIntegral <$> (Maybe.listToMaybe $ H.decode ctx $ T.unpack r)

instance (ToBackendKey SqlBackend e) => Obfuscateable (Key e) where
  type Obfuscated (Key e) = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral $ fromSqlKey i]
  deobfuscate ctx r = (toSqlKey . fromIntegral) <$> (Maybe.listToMaybe $ H.decode ctx $ T.unpack r)

data ObfuscatedCapture (sym :: Symbol) (a :: Type) 
  deriving (Typeable)

instance ( Obfuscateable a
         , Obfuscated a ~ T.Text
         , HasContextEntry context H.HashidsContext
         , KnownSymbol capture
         , HasServer api context
         ) => HasServer (ObfuscatedCapture capture a :> api) context where

  type ServerT (ObfuscatedCapture capture a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case deobfuscate hashidsContext txt of
                  Just v -> return v 
                  Nothing -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
            )

data ObfuscatedReqBody (contentType :: [Type]) (a :: Type)
  deriving (Typeable)

instance {-# OVERLAPPING #-}
      ( Obfuscateable a
      , AllCTUnrender ctypes (Obfuscated a)
      , HasContextEntry context H.HashidsContext
      , HasServer api context
      ) => HasServer (ObfuscatedReqBody ctypes a :> api) context where
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

instance (FromJSON a, HasContextEntry context H.HashidsContext, HasServer api context) 
       => HasServer (ObfuscatedReqBody '[JSON] a :> api) context where
  type ServerT (ObfuscatedReqBody '[JSON] a :> api) m = a -> ServerT api m

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
        case decodeFields (getContextEntry context) <$> mrqbody of
            Left e            -> delayedFailFatal err400 { errBody = cs e }
            Right (Error e)   -> delayedFailFatal err400 { errBody = cs e }
            Right (Success v) -> return v

data ObfuscatedVerb (method :: StdMethod) (status :: Nat) (ctypes :: [*]) (a :: *)
  deriving Typeable

instance ( AllCTRender ctypes (Obfuscated a)
         , Obfuscateable a
         , ToJSON (Obfuscated a)
         , HasContextEntry context H.HashidsContext
         , ReflectMethod method
         , KnownNat status
         ) => HasServer (ObfuscatedVerb method status ctypes a) context where
  type ServerT (ObfuscatedVerb method status ctypes a) m = ServerT (Verb method status ctypes a) m
  hoistServerWithContext _ _ nt s = nt s
  route Proxy context = methodRouter obfuscateBody method (Proxy :: Proxy ctypes) status
        where method = reflectMethod (Proxy :: Proxy method)
              status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)
              obfuscateBody b = ([], obfuscate (getContextEntry context) b)

type ObfuscatedGet   = ObfuscatedVerb GET   200 
type ObfuscatedPost  = ObfuscatedVerb POST  200
type ObfuscatedPut   = ObfuscatedVerb PUT   200 
type ObfuscatedPatch = ObfuscatedVerb PATCH 200 

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
