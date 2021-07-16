module Web.AppHandler
  ( AppHandler
  , AppServer
  , Env
  , runAppHandler
  ) where

import           Control.Applicative
import           Control.Exception.Extended (Exception, SomeException (..),
                                             fromException, handle,
                                             mapException, throwIO, try)
import           Control.Monad.Except
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Lazy.Char8 as LC8
import           Env
import           Servant
import           UnliftIO                   (MonadUnliftIO)

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

runAppHandler :: Env -> AppHandler a -> IO a
runAppHandler env =
    flip runReaderT env . unAppHandler

    {--
  Handler . ExceptT . try . (mapException toServantServerError) . flip runReaderT env . unAppHandler
    where
      toServantServerError e@(SomeException _)
        | Just (ServerError _ _ _ _) <- fromException e = e
        | envDebug env == True = SomeException $ err500{errBody=LC8.pack $ show e}
        | otherwise = e
 --}
