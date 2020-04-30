module Web.Auth (Protected, SessionAuthHandler, authHandler) where

import Servant.API
import Servant.Server.Experimental.Auth
import Network.Wai
import Env
import DB.Model.User
import DB.Session
import Control.Monad.IO.Class
import Web.Errors
import Data.SessionData
  
type Protected = AuthProtect "session-cookie"

type SessionAuthHandler = AuthHandler Request SessionData

type instance AuthServerData (Protected) = SessionData

authHandler :: Env -> SessionAuthHandler
authHandler env = mkAuthHandler handler
  where
    handler req = do
      sessionKey <- maybeUnauthorized $ lookup "hs-forum-session-key" $ requestHeaders req
      user <- maybeUnauthorized =<< fetchSession (redisConn env) (sessionKey)
      pure $ SessionData { sessionUser = user }
