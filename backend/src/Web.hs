{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Web where

import qualified Config
import           Data.Aeson
import           Env
import           Hashids
import qualified Network.Wai              as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Web.AppHandler
import           Web.Auth
import qualified Web.Endpoint.Forum       as Forum
import qualified Web.Endpoint.Session     as Session
import qualified Web.Endpoint.User        as User
import           Web.Obfuscate

import           Control.Monad.Reader     (MonadReader)
import           Domain.Types.SessionData (SessionData (..))
import           Web.Eved
import           Web.Eved.Auth

type Api m =
           Session.Api m
      :<|> Forum.Api m
     --  :<|> User.Api

api ::
    ( Eved api m
    , EvedAuth api
    , MonadReader ctx r
    , HasAuthScheme ctx SessionData
    ) => r (api (Api m))
api = Session.api .<|> Forum.api -- :<|> User.api

handler :: Api AppHandler
handler = Session.server :<|> Forum.server -- :<|> User.server

mkApp :: Config.Config -> IO Wai.Application
mkApp config =
  withEnv config $ \env ->
  let Right hashidsCtx = mkHashidsContext "" 6 defaultAlphabet
  in pure $ server (runAppHandler env) handler (api (sessionAuth env))

runApp :: IO ()
runApp = do
    config <- Config.loadConfigFile
    app <- mkApp config
    putStrLn $ "Running on " <> (show $ Config.configPort config)
    Warp.run (Config.configPort config) app
