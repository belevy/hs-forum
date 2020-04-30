module Web.AppHandler 
  ( AppHandler
  , AppServer
  , Env
  , runAppHandler
  ) where

import Servant
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Except (ExceptT(..))
import UnliftIO (try, MonadUnliftIO)
import Env

type AppServer api = ServerT api AppHandler

newtype AppHandler a = AppHandler 
  { unAppHandler :: ReaderT Env IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadUnliftIO)

runAppHandler :: Env -> AppHandler a -> Handler a
runAppHandler env = 
  Handler . ExceptT . try . flip runReaderT env . unAppHandler 
  
