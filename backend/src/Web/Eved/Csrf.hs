module Web.Eved.Csrf
    where

import           Control.Monad
import           Data.Cookie
import qualified Data.Maybe         as Maybe
import           Network.Wai
import           UnliftIO.Exception (throwIO)
import           Web.Cookie
import           Web.Errors
import           Web.Eved.Server

withCsrfToken :: (Functor f, EvedCsrf api) => f (api a) -> f (api a)
withCsrfToken = fmap withCsrfToken_

checkCsrfToken :: (Functor f, EvedCsrf api) => f (api a) -> f (api a)
checkCsrfToken = fmap checkCsrfToken_

class EvedCsrf api where
    withCsrfToken_ :: api a -> api a
    checkCsrfToken_ :: api a -> api a

instance EvedCsrf (EvedServerT m) where
    withCsrfToken_ next =
        EvedServerT $ \nt path action req respond ->
            unEvedServerT next nt path action req $ \response -> do
                cookie <- csrfCookie <$> generateRandomToken
                respond $ mapResponseHeaders (renderCookieHeader cookie:) response

    checkCsrfToken_ next =
        EvedServerT $ \nt path action request respond -> do
            let headerToken = lookup "x-csrf-token" (requestHeaders request)
                cookieToken = getCookie request "csrf-token"

            when (Maybe.isNothing cookieToken || headerToken /= cookieToken) $ do
                throwIO err403{errorBody = "CSRF tokens didnt match or was expired"}

            unEvedServerT next nt path action request respond
     where
      getCookie request cookie = do
        cookies <- lookup "cookie" (requestHeaders request)
        lookup cookie $ parseCookies cookies
