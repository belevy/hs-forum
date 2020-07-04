module Web.Auth (Protected, SessionAuthHandler, authHandler) where

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Env
import DB.Session
import Web.Errors
import Web.Cookie
import Domain.Types.SessionData (SessionData(..))
  
type Protected = AuthProtect "session-cookie"

type SessionAuthHandler = AuthHandler Request SessionData

type instance AuthServerData (Protected) = SessionData

authHandler :: Env -> SessionAuthHandler
authHandler env = mkAuthHandler handler
  where
    handler req = do
      cookies <- maybeThrowError err400 $ lookup "cookie" $ requestHeaders req
      sessionKey <- maybeUnauthorized $ lookup "hs-forum-session-key" $ parseCookies cookies 
      session <- fetchSession (redisConn env) (sessionKey)
      maybe (throwError err401{errBody="Session Not Found"}) pure session 
