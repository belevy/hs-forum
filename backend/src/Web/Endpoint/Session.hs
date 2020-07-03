module Web.Endpoint.Session
  ( Api
  , server
  ) where

import Servant
import Web.Auth
import Web.Obfuscate
import Web.AppHandler
import Domain.Types.SessionData
import DB.User
import DB.Session
import Data.UserCredentials
import Env
import Control.Monad.Reader (asks)
import Web.Errors
import Database.Persist (Entity(..))
import Web.Cookie
import Web.Servant.Csrf

type Api = "sessions" :>
  (     Protected :> Obfuscate :> Get '[JSON] SessionData
   :<|> ReqBody '[JSON] UserCredentials :> Post '[JSON] (Headers '[CSRFTokenCookie, Header "Set-Cookie" SetCookie] ())
  )

server :: AppServer Api
server = currentSession :<|> login
  where
    currentSession :: SessionData -> AppHandler (SessionData)
    currentSession session = pure session

    login :: UserCredentials -> AppHandler (Headers '[CSRFTokenCookie, Header "Set-Cookie" SetCookie] ())
    login creds = do
      conn <- asks redisConn
      mUser <- runDB $ fetchAndVerifyUser creds
      user <- maybeThrowError err403 mUser
      sessionCookie <- createSession conn (entityVal user) (60*60*24*30)
      addCsrfToken $ addHeader sessionCookie ()
