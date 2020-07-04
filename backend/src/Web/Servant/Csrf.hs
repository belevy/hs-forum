{-# LANGUAGE DeriveDataTypeable #-}

module Web.Servant.Csrf 
  where

import Servant
import Servant.Server
import Servant.Server.Internal
import Web.Cookie
import Network.Wai

import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Cookie
import Data.String (fromString)
import qualified Data.Maybe as Maybe


type CSRFTokenCookie = Header "Set-Cookie" SetCookie

addCsrfToken :: (AddHeader "Set-Cookie" SetCookie a a', MonadIO m) => a -> m a'
addCsrfToken a = do
  cookie <- csrfCookie <$> generateRandomToken 
  pure $ addHeader cookie a

type family WithCSRFToken a where
  WithCSRFToken (Headers headers a) = Headers (CSRFTokenCookie ': headers) a
  WithCSRFToken a = Headers '[CSRFTokenCookie] a

type family CSRFProtected a 
type instance CSRFProtected (a :> b) = a :> CSRFProtected b
type instance CSRFProtected (a :<|> b) = CSRFProtected a :<|> CSRFProtected b 
type instance CSRFProtected (Verb method status ctypes a) = Verb method status ctypes (WithCSRFToken a)

data CheckCSRF 
  deriving Typeable

instance HasServer api context => HasServer (CheckCSRF :> api) context where
  type ServerT (CheckCSRF :> api) m = ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

  route Proxy context d = 
    route (Proxy :: Proxy api) context $
      addHeaderCheck_ d $ withRequest $ \request -> do
        let headerToken = lookup "x-csrf-token" $ requestHeaders request
            cookieToken = getCookie request "csrf-token"
        when (Maybe.isNothing cookieToken || headerToken /= cookieToken) $ do
          delayedFail err403{errBody = "CSRF tokens didnt match or was expired"}

    where
      addHeaderCheck_ :: Delayed env a -> DelayedIO () -> Delayed env a
      addHeaderCheck_ d = addHeaderCheck (fmap const d)

      getCookie :: Request -> ByteString -> Maybe ByteString
      getCookie request cookie = do
        cookies <- lookup "cookie" (requestHeaders request)
        lookup cookie $ parseCookies cookies


