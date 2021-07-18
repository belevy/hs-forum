{-# LANGUAGE OverloadedLists #-}
module Web.Endpoint.User
  where

import           UnliftIO.Exception   (throwIO)

import           Env
import           Web.AppHandler
import           Web.Errors
import           Web.Eved
import qualified Web.Eved.ContentType as CT
import           Web.Eved.Server

import           DB.User
import           Data.UserCredentials

type Api m = UserCredentials -> m ()

api :: (Applicative f, Eved api m) => f (api (Api m))
api = lit "users" .</> reqBody [CT.json @UserCredentials] .</> post [CT.json @()]

server :: Api AppHandler
server = createUser
  where
    createUser :: UserCredentials -> AppHandler ()
    createUser credentials = do
      eUser <- runDB $ registerUser credentials
      either handleRegistrationError (const $ pure ()) eUser

      where
        handleRegistrationError UserAlreadyExists = throwIO err409{errorBody="That username is taken"}
        handleRegistrationError e = throwIO err500
