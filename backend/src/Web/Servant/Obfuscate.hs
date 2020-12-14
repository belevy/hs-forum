{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Web.Servant.Obfuscate where

import           Control.Applicative      ((<|>))
import           Control.Monad
import           Control.Monad.IO.Class   (liftIO)
import           Data.Int                 (Int64)
import           Data.Kind
import           Data.List                (nub)
import qualified Data.Maybe               as Maybe
import           Data.String.Conversions  (cs)
import           Data.Tagged
import qualified Data.Text                as T
import           Data.Typeable            (Typeable)
import           GHC.TypeLits             (KnownNat, KnownSymbol, Nat, Symbol,
                                           natVal)

import           Data.Aeson
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.HashMap.Strict      (mapWithKey)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Scientific          (isInteger, toBoundedInteger)
import           Database.Persist
import           Database.Persist.Sql
import qualified Hashids                  as H
import qualified Language.Haskell.TH      as TH
import           Network.HTTP.Types       (HeaderName, Method, Status, hAccept,
                                           hContentType)
import           Network.Wai              (lazyRequestBody, requestHeaders,
                                           responseLBS)
import           Servant
import           Servant.API.ContentTypes
import           Servant.Server.Internal
import           Web.HttpApiData
import           Web.Obfuscate

data Obfuscate deriving Typeable

class HasObfuscatedServerImplementation api context where
  routeImpl :: Proxy api -> Context context -> Delayed env (Server api) -> Router env
  hoistServerWithContextImpl :: Proxy api -> Proxy context -> (forall x. m x -> n x) -> ServerT api m -> ServerT api n

instance ( CanDeobfuscate a
         , Obfuscated a ~ Value
         , HasContextEntry context H.HashidsContext
         , KnownSymbol capture
         , HasServer api context
         ) => HasObfuscatedServerImplementation (Capture (capture :: Symbol) a :> api) context where

  hoistServerWithContextImpl _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  routeImpl Proxy context d = CaptureRouter $
      route (Proxy :: Proxy api) context $
            addCapture d $ \txt ->
               case deobfuscate (getContextEntry context) (String txt) of
                  Just v -> return v
                  Nothing -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }

instance ( CanDeobfuscate a
         , HasContextEntry context H.HashidsContext
         , AllMimeUnrender ctypes (Obfuscated a)
         , HasServer api context
         ) => HasObfuscatedServerImplementation (ReqBody ctypes a :> api) context where
  hoistServerWithContextImpl _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s
  routeImpl Proxy context subserver =
    route (Proxy :: Proxy api) context $
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

