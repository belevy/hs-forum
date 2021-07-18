module Web.Auth (HasAuthScheme(..), protected, AuthScheme, SessionData, sessionAuth) where

import           DB.Session                       (findSession)
import           Domain.Types.SessionData         (SessionData (..))
import           Env
import           Network.Wai
import           Servant
import           Servant.Server.Experimental.Auth
import           Web.Cookie
import           Web.Errors

import           Control.Monad.Reader             (MonadReader, asks)
import           Web.Eved                         (Eved)
import           Web.Eved.Auth

class HasAuthScheme env authData where
    getAuthScheme :: env -> AuthScheme authData

instance HasAuthScheme (AuthScheme authData) authData where
    getAuthScheme = id

instance HasAuthScheme (AuthScheme authData, b) authData where
    getAuthScheme = fst
instance HasAuthScheme b authData => HasAuthScheme (a, b) authData where
    getAuthScheme = getAuthScheme . snd

protected ::
    ( MonadReader env f
    , HasAuthScheme env SessionData
    , Eved api m
    , EvedAuth api
    ) => f (api a)
      -> f (api (SessionData -> a))
protected =
    auth $ pure (asks getAuthScheme)

sessionAuth :: Env -> AuthScheme SessionData
sessionAuth env =
    AuthScheme
        { authenticateRequest = \req -> do -- authentication may require IO this should be modified to support running authentications in IO
              case lookup "cookie" $ requestHeaders req of
                Nothing -> pure AuthNeeded
                Just cookies -> do
                    case lookup "hs-forum-session-key" $ parseCookies cookies of
                      Nothing -> pure $ AuthFailure "Missing sessionKey header"
                      Just sessionKey -> do
                          mSession <- findSession (redisConn env) sessionKey
                          case mSession of
                            Nothing -> pure $ AuthFailure "No session found for sessionKey"
                            Just a  -> pure $ AuthSuccess a

        , addCredentials = error "addCredentials undefined for sessionAuth AuthScheme"
        }
