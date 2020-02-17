module Web where

import Control.Monad (Monad)
import Servant
import qualified Network.Wai.Handler.Warp as Warp
import Data.UserCredentials (UserCredentials)
import qualified Data.UserCredentials as UserCredentials
import Control.Monad.Reader
import Control.Monad.Except
import Database.Persist.Postgresql
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)
import Control.Monad.Logger (runNoLoggingT)
import Data.UserCredentials
import DB.User
import qualified Config
import Data.String

type Api = "session" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] ()

type Env = Pool SqlBackend

newtype AppHandler a = AppHandler 
  { unAppHandler :: ReaderT Env Handler a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env)

server :: ServerT Api AppHandler
server = \userCredentials -> do
  liftIO $ print userCredentials
  pool <- ask
  liftIO $ flip runSqlPool pool $ do
    user <- fetchAndVerifyUser userCredentials 
    liftIO $ print user
    pure ()

api :: Proxy Api
api = Proxy

app :: Env -> Application
app pool = serve api (hoistServer api (flip runReaderT pool . unAppHandler) server)

runApp :: IO ()
runApp = do
  config <- Config.loadConfigFile
  runNoLoggingT $ 
    withPostgresqlPool (Config.dbConnectionString config) 1 $ \pool -> do
    liftIO $ putStrLn $ "Running on " <> (show $ Config.configPort config)
    liftIO $ Warp.run (Config.configPort config) (app pool)
