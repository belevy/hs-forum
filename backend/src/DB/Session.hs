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
import DB.User
import Database.Redis as Redis
import Data.Cookie
import Data.SessionData as SessionData

fetchSession :: MonadIO m => Redis.Connection -> Token -> m (Maybe SessionData)
fetchSession conn sessionKey = do
  eSession <- liftIO $ runRedis conn $ Redis.get sessionKey
  pure $ either (\_ -> Nothing) (SessionData.decode =<<) eSession

createSession :: MonadIO m => Redis.Connection -> User -> Integer -> m Token
createSession conn user expiration = liftIO $ do
  sessionKey <- generateRandomToken
  runRedis conn $ Redis.setex sessionKey expiration (SessionData.encode $ SessionData user)
  pure sessionKey
  where
    toMd5ByteString :: String -> C8.ByteString
    toMd5ByteString =
      md5DigestBytes . md5 . LC8.pack

