module Web.Auth (Protected, SessionAuthHandler, authHandler) where

import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Env
import DB.Session
import Web.Errors
import Data.SessionData
import Web.Cookie
  
type Protected = AuthProtect "session-cookie"

type SessionAuthHandler = AuthHandler Request SessionData

type instance AuthServerData (Protected) = SessionData

authHandler :: Env -> SessionAuthHandler
authHandler env = mkAuthHandler handler
  where
    handler req = do
      cookies <- maybeThrowError err400 $ lookup "cookie" $ requestHeaders req
      sessionKey <- maybeUnauthorized $ lookup "hs-forum-session-key" $ parseCookies cookies 
      maybeUnauthorized =<< fetchSession (redisConn env) (sessionKey)
