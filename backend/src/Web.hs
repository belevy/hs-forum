{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Web where

import Servant
import qualified Network.Wai.Handler.Warp as Warp
import qualified Config
import Env
import Web.AppHandler
import qualified Web.Endpoint.Forum as Forum
import qualified Web.Endpoint.Session as Session
import qualified Web.Endpoint.User as User
import Web.Auth
import Hashids
import Data.Aeson
import Web.Obfuscate

type Api = Session.Api 
      :<|> Forum.Api
      :<|> User.Api

server :: AppServer Api 
server = Session.server :<|> Forum.server :<|> User.server

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
