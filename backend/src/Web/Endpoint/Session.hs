{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Endpoint.Session
  ( Api
  , server
  , api
  ) where

import           DB.Session
import           DB.User
import           Data.Cookie
import           Data.UserCredentials
import           Database.Persist         (Entity (..))
import           Domain.Types.SessionData as Session
import           Env
import           Web.AppHandler
import           Web.Auth
import           Web.Errors

import           Control.Monad.Reader
import           Web.Eved                 hiding (server)
import           Web.Eved.Auth            (AuthScheme, EvedAuth (..), auth)
import qualified Web.Eved.ContentType     as CT

type Api m =
      (SessionData -> m SessionResponse)
 :<|> (UserCredentials -> m (CT.WithHeaders ()))

api :: ( MonadReader ctx r
       , HasAuthScheme ctx SessionData
       , Eved api m
       , EvedAuth api
       ) => r (api (Api m))
api =
    lit "sessions" .</>
      (     (lit "me" .</> protected .</> get [CT.json])
       .<|> (reqBody [CT.json] .</> post [CT.withHeaders CT.json])
      )

server :: Api AppHandler
server = currentSession :<|> login
  where
    currentSession :: SessionData -> AppHandler SessionResponse
    currentSession session =
        pure $ Session.fromModel session

    login :: UserCredentials -> AppHandler (CT.WithHeaders ())
    login creds = do
      conn <- asks redisConn
      mUser <- runDB $ findAndVerifyUser creds
      user <- maybeThrowError err403 mUser
      sessionCookie <- createSession conn (entityVal user) (60*60*24*30)
      pure $ CT.addHeaders [renderCookieHeader sessionCookie] ()
