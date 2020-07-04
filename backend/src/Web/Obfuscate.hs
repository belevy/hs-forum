{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

class Obfuscateable a where
  type Obfuscated a
  obfuscate :: H.HashidsContext -> a -> Obfuscated a
  deobfuscate :: H.HashidsContext -> Obfuscated a -> Maybe a

deobfuscateIntegral :: (Read a, Integral a) => H.HashidsContext -> T.Text -> Maybe a
deobfuscateIntegral ctx r = fromIntegral <$>
  (Maybe.listToMaybe $ H.decode ctx $ T.unpack r) <|> (readMaybe $ T.unpack r)

instance Obfuscateable a => Obfuscateable [a] where
  type Obfuscated [a] = [Obfuscated a] 
  obfuscate ctx xs = fmap (obfuscate ctx) xs
  deobfuscate ctx ys = traverse (deobfuscate ctx) ys

instance Obfuscateable Integer where
  type Obfuscated Integer = T.Text
  obfuscate ctx i = T.pack $ H.encode ctx [i]
  deobfuscate = deobfuscateIntegral

instance Obfuscateable Int where
  type Obfuscated Int = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral i]
  deobfuscate = deobfuscateIntegral

instance Obfuscateable Int64 where
  type Obfuscated Int64 = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral i]
  deobfuscate = deobfuscateIntegral

instance (ToBackendKey SqlBackend e) => Obfuscateable (Key e) where
  type Obfuscated (Key e) = T.Text

  obfuscate ctx i = T.pack $ H.encode ctx [fromIntegral $ fromSqlKey i]
  deobfuscate ctx r = toSqlKey <$> deobfuscateIntegral ctx r

instance Obfuscateable a => Obfuscateable (Headers ls a) where
  type Obfuscated (Headers ls a) = Obfuscated a

  obfuscate ctx (Headers a hlist)= obfuscate ctx a
  deobfuscate ctx _ = undefined


class HasObfuscatedServerImplementation api context where
  routeImpl :: Proxy api -> Context context -> Delayed env (Server api) -> Router env 
  hoistServerWithContextImpl :: Proxy api -> Proxy context -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n 

instance ( Obfuscateable a
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

instance ( Obfuscateable a
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

instance ( ReflectMethod method, KnownNat status
         , HasContextEntry context H.HashidsContext
         , Obfuscateable a
         , AllCTRender ctypes (Obfuscated a)
         ) => HasObfuscatedServerImplementation (Verb (method :: StdMethod) (status :: Nat) (ctypes :: [*]) a) context where
  hoistServerWithContextImpl _ _ nt s = nt s

  routeImpl Proxy ctx = 
    methodRouter (\x -> ([], obfuscate (getContextEntry ctx) x)) method (Proxy :: Proxy ctypes) status
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
