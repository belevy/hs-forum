module Data.Cookie
    ( module Web.Cookie
    , module Data.Cookie
    ) where

import           Control.Monad.IO.Class
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Base64  as BS64
import           Data.ByteString.Builder as Builder
import           Data.ByteString.Lazy    as LBS
import           Data.Time.Clock
import           Network.HTTP.Types      (Header)
import           System.Entropy          (getEntropy)
import           Web.Cookie

type Token = BS.ByteString

generateRandomToken :: MonadIO m => m Token
generateRandomToken =
  liftIO $ BS64.encode <$> getEntropy 32

renderCookieHeader :: SetCookie -> Header
renderCookieHeader setCookie =
    ("Set-Cookie", LBS.toStrict $ Builder.toLazyByteString $ renderSetCookie setCookie)

csrfCookie :: Token -> SetCookie
csrfCookie token =
  defaultSetCookie
    { setCookieName = "csrf-token"
    , setCookieValue = token
    , setCookieMaxAge = Just thirtyMinutes
    , setCookiePath = Just "/api"
    }
  where
    thirtyMinutes =
      secondsToDiffTime $ 60 * 30

