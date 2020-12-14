module Web.Endpoint.Session
  ( Api
  , server
  ) where

import Servant
import Web.Auth
import Web.AppHandler
import Domain.Types.SessionData as Session
import DB.User
import DB.Session
import Data.UserCredentials
import Env
import Control.Monad.Reader (asks)
import Web.Errors
import Database.Persist (Entity(..))
import Web.Cookie
import Web.Servant.Csrf
import Web.Servant.Obfuscate

type Api = "sessions" :> 
  (     "me" :> Protected :> Get '[JSON] (WithCSRFToken SessionResponse)
   :<|> "csrf-token" :> Protected :> Get '[JSON] (WithCSRFToken ())
   :<|> ReqBody '[JSON] UserCredentials :> Post '[JSON] (Headers '[CSRFTokenCookie, Header "Set-Cookie" SetCookie] ())
  )

server :: AppServer Api
server = currentSession :<|> getCsrfToken :<|> login
  where
    currentSession :: SessionData -> AppHandler (WithCSRFToken SessionResponse)
    currentSession session = addCsrfToken $ Session.fromModel session
  
    getCsrfToken :: SessionData -> AppHandler (WithCSRFToken ())
    getCsrfToken _ = addCsrfToken ()

    login :: UserCredentials -> AppHandler (WithCSRFToken (Headers '[Header "Set-Cookie" SetCookie] ()))
    login creds = do
      conn <- asks redisConn
      mUser <- runDB $ fetchAndVerifyUser creds
      user <- maybeThrowError err403 mUser
      sessionCookie <- createSession conn (entityVal user) (60*60*24*30)
      addCsrfToken $ addHeader sessionCookie ()
