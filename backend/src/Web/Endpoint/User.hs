module Web.Endpoint.User 
  where

import Servant
import UnliftIO.Exception (throwIO)

import Web.AppHandler
import Env

import Data.UserCredentials
import DB.User
    
type Api = "users" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] ()

server :: AppServer Api
server = createUser 
  where
    createUser :: UserCredentials -> AppHandler ()
    createUser credentials = do
      eUser <- runDB $ registerUser credentials
      either handleRegistrationError (const $ pure ()) eUser

      where
        handleRegistrationError UserAlreadyExists = throwIO err409{errBody="That username is taken"}
        handleRegistrationError e = throwIO err500
