module Data.Cookie where 

import Control.Monad.IO.Class
import qualified Data.ByteString   as BS
import qualified Data.ByteString.Base64   as BS64
import Web.Cookie
import Data.Time.Clock
import System.Entropy (getEntropy)

type Token = BS.ByteString

generateRandomToken :: MonadIO m => m Token 
generateRandomToken =
  liftIO $ BS64.encode <$> getEntropy 32

csrfCookie :: Token -> SetCookie
csrfCookie token = 
  defaultSetCookie
    { setCookieName = "csrf-token"
    , setCookieValue = token
    , setCookieMaxAge = Just thirtyMinutes
    }
  where 
    thirtyMinutes =
      secondsToDiffTime $ 60 * 30 

