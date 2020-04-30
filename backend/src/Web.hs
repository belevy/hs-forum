{-# LANGUAGE TemplateHaskell #-}

module Web where

import Control.Monad 
import Control.Monad.IO.Class (MonadIO, liftIO)
import Servant
import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp
import Data.UserCredentials (UserCredentials)
import qualified Data.UserCredentials as UserCredentials
import Data.UserCredentials
import DB.User
import DB.Model.User (UserId)
import qualified Config
import Env
import Web.AppHandler
import Database.Persist.Postgresql
import Web.Obfuscate
import qualified Data.Text as T
import Data.Aeson (defaultOptions, camelTo2, fieldLabelModifier)
import Data.Aeson.TH
import qualified Web.Endpoint.Forum as Forum
import qualified Web.Endpoint.Session as Session
import Web.Auth
import Data.SessionData
import Hashids

type Api = Session.Api 
     :<|> Forum.Api

server :: AppServer Api 
server = Session.server :<|> Forum.server

api :: Proxy Api
api = Proxy

mkApp :: Config.Config -> IO Application
mkApp config = 
  withEnv config $ \env -> 
  let Right hashidsCtx = mkHashidsContext "" 6 defaultAlphabet
      context = authHandler env :. hashidsCtx :. EmptyContext
      contextProxy = Proxy @'[SessionAuthHandler, HashidsContext]
  in do 
    pure $ serveWithContext api context
          $ hoistServerWithContext api contextProxy (runAppHandler env) server
           
runApp :: IO ()
runApp = do
    config <- Config.loadConfigFile
    app <- mkApp config
    putStrLn $ "Running on " <> (show $ Config.configPort config)
    Warp.run (Config.configPort config) app 
