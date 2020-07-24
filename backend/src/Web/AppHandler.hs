module Web.AppHandler 
  ( AppHandler
  , AppServer
  , Env
  , runAppHandler
  ) where

import Servant
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 as LC8
import Control.Monad.IO.Class
import Control.Monad.Except 
import Control.Monad.Fail
import Control.Applicative
import UnliftIO (MonadUnliftIO)
import Control.Exception.Extended (try, SomeException(..), Exception, handle, throwIO, fromException)
import Control.Exception (mapException)
import Env

type AppServer api = ServerT api AppHandler

newtype AppHandler a = AppHandler 
  { unAppHandler :: ReaderT Env IO a
  } deriving newtype ( Functor
                     , Applicative
                     , Alternative
                     , Monad
                     , MonadIO
                     , MonadPlus
                     , MonadReader Env
                     , MonadUnliftIO
                     , MonadFail
                     )

runAppHandler :: Env -> AppHandler a -> Handler a
runAppHandler env = 
  Handler . ExceptT . try . (mapException toServantServerError) . flip runReaderT env . unAppHandler 
    where
      toServantServerError e@(SomeException _)
        | Just (ServerError _ _ _ _) <- fromException e = e
        | envDebug env == True = SomeException $ err500{errBody=LC8.pack $ show e}
        | otherwise = e
  
