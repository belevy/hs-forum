module Web where

import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO)
import Servant
import qualified Network.Wai.Handler.Warp as Warp
import Data.UserCredentials (UserCredentials)
import qualified Data.UserCredentials as UserCredentials
import Data.UserCredentials
import DB.User
import qualified Config
import Env
import Web.AppHandler
import Database.Persist.Postgresql

type Api = "session" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] ()

server :: AppServer Api 
server = \userCredentials -> do
  liftIO $ print userCredentials
  runDB $ do
    user <- fetchAndVerifyUser userCredentials 
    liftIO $ print user
    pure ()

api :: Proxy Api
api = Proxy

mkApp :: Config.Config -> IO Application
mkApp config = 
  withEnv config $ \env -> 
  pure $ serve api $ hoistServer api (runAppHandler env) server

runApp :: IO ()
runApp = do
    config <- Config.loadConfigFile
    app <- mkApp config
    putStrLn $ "Running on " <> (show $ Config.configPort config)
    Warp.run (Config.configPort config) app 
