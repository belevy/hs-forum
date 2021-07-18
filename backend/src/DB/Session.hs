module DB.Session
    (findSession, createSession)
    where

import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           DB.Model.User
import           Data.Cookie
import           Data.Time.Clock          (addUTCTime, getCurrentTime)
import           Database.Redis           as Redis
import           Domain.Types.SessionData (SessionData (..))
import qualified Domain.Types.SessionData as SessionData
import           Web.Cookie

findSession :: MonadIO m => Redis.Connection -> Token -> m (Maybe SessionData)
findSession conn sessionKey = do
  eSession <- liftIO $ runRedis conn $ Redis.get sessionKey
  pure $ either (const Nothing) (SessionData.decode =<<) eSession

createSession :: MonadIO m => Redis.Connection -> User -> Integer -> m SetCookie
createSession conn user expiration = liftIO $ do
  sessionKey <- generateRandomToken
  now <- getCurrentTime
  runRedis conn $ Redis.setex sessionKey expiration (SessionData.encode $ SessionData user)
  pure $ defaultSetCookie
      { setCookieName = "hs-forum-session-key"
      , setCookiePath = Just "/"
      , setCookieValue = sessionKey
      , setCookieMaxAge = Just (fromIntegral expiration)
      , setCookieExpires = Just $ addUTCTime (fromIntegral expiration) now
      , setCookieHttpOnly = True
      , setCookieSecure = True
      , setCookieSameSite = Just sameSiteStrict
      }

