module Web.Auth (Protected, SessionAuthHandler, SessionData(..), authHandler) where

import Servant.API
import Servant.Server.Experimental.Auth
import Network.Wai
import Env
import DB.Model.User
import DB.Session
import Control.Monad.IO.Class
import Web.Errors
  
type Protected = AuthProtect "session-cookie"

data SessionData = SessionData 
  { sessionUser :: User }

type SessionAuthHandler = AuthHandler Request SessionData

type instance AuthServerData (Protected) = SessionData

authHandler :: Env -> SessionAuthHandler
authHandler env = mkAuthHandler handler
  where
    handler req = do
      sessionKey <- maybeUnauthorized $ lookup "hs-forum-session-key" $ requestHeaders req
      user <- maybeUnauthorized =<< fetchSession (redisConn env) (sessionKey)
      pure $ SessionData { sessionUser = user }
