module Web.Endpoint.Session 
  ( Api
  , server
  ) where

import Servant
import Web.Auth
import Web.Obfuscate
import Web.AppHandler
import Data.SessionData
import DB.User
import DB.Session
import Data.UserCredentials
import Env
import Control.Monad.Reader (asks)
import Web.Errors
import Database.Persist (Entity(..))
import Web.Cookie

type Api = "sessions" :> 
  (    Protected :> ObfuscatedGet '[JSON] SessionData
  :<|> ReqBody '[JSON] UserCredentials :> Post '[JSON] (Headers '[Header "set-cookie" SetCookie] ())
  )

server :: AppServer Api
server = currentSession :<|> login
  where
    currentSession session = pure session
    login creds = do 
      conn <- asks redisConn
      mUser <- runDB $ fetchAndVerifyUser creds 
      user <- maybeThrowError err403 mUser
      sessionKey <- createSession conn (entityVal user) (60*60*24*30)
      pure $ addHeader (defaultSetCookie
          { setCookieName = "hs-forum-session-key"
          , setCookieValue = sessionKey 
          , setCookieMaxAge = Just (60*60*24*30)
          }) ()
