module Env 
  ( Env
  , withEnv
  , runDB
  , runDBReadOnly
  ) where

import Database.Persist.Sql
import Data.Pool
import Config
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger, LoggingT, runStdoutLoggingT)
import UnliftIO (MonadUnliftIO)

data Env = Env 
  { envPool :: Pool SqlBackend
  }

runDBReadOnly :: (MonadReader Env m, MonadUnliftIO m) => SqlReadT m a -> m a
runDBReadOnly = runDB

runDB :: (MonadReader Env m, MonadUnliftIO m) => SqlPersistT m a -> m a
runDB query = do
  pool <- asks envPool
  runSqlPool query pool

withEnv :: MonadUnliftIO m 
        => Config 
        -> (Env -> LoggingT m a) 
        -> m a
withEnv config action =
  runStdoutLoggingT $
  withPostgresqlPool (dbConnectionString config) 1 $ action . Env

