module DB.Session where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Time.Clock
import Data.String
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Digest.Pure.MD5

import Data.UserCredentials
import DB.Model.User
import DB.Model.Session (Session, SessionKey)
import DB.User
import qualified DB.Model.Session as Session
import Database.Redis as Redis
import Data.Cookie

fetchSession :: MonadIO m => Redis.Connection -> SessionKey -> m (Maybe User)
fetchSession conn sessionKey = do
  eSession <- liftIO $ runRedis conn $ Redis.get sessionKey
  pure $ either (\_ -> Nothing) (\mSession -> Session.decode =<< mSession) eSession

createSession :: MonadIO m => Redis.Connection -> User -> Integer -> m SessionKey
createSession conn user expiration = liftIO $ do
  sessionKey <- generateRandomToken
  runRedis conn $ Redis.setex sessionKey expiration (Session.encode user)
  pure sessionKey
  where
    toMd5ByteString :: String -> C8.ByteString
    toMd5ByteString =
      md5DigestBytes . md5 . LC8.pack
