module DB.Session where

import Control.Monad.IO.Class (MonadIO, liftIO)
import DB.Model.User
import Database.Redis as Redis
import Data.Cookie
import Web.Cookie
import Domain.Types.SessionData (SessionData(..))
import qualified Domain.Types.SessionData as SessionData

fetchSession :: MonadIO m => Redis.Connection -> Token -> m (Maybe SessionData)
fetchSession conn sessionKey = do
  eSession <- liftIO $ runRedis conn $ Redis.get sessionKey
  pure $ either (\_ -> Nothing) (SessionData.decode =<<) eSession

createSession :: MonadIO m => Redis.Connection -> User -> Integer -> m SetCookie
createSession conn user expiration = liftIO $ do
  sessionKey <- generateRandomToken
  runRedis conn $ Redis.setex sessionKey expiration (SessionData.encode $ SessionData user)
  pure $ defaultSetCookie
      { setCookieName = "hs-forum-session-key"
      , setCookieValue = sessionKey 
      , setCookieMaxAge = Just (fromIntegral expiration)
      }

