module Env 
  ( Env(..)
  , withEnv
  , runDB
  , runDBReadOnly
  ) where

import Database.Persist.Sql
import Data.Pool
import qualified Data.Maybe as Maybe
import Database.Persist.Postgresql
import Control.Monad.Reader
import Control.Monad.Logger (MonadLogger, LoggingT, runStdoutLoggingT)
import UnliftIO (MonadUnliftIO, withRunInIO)
import qualified Database.Redis as Redis

import Config

data Env = Env 
  { envPool :: Pool SqlBackend
  , envDebug :: Bool
  , redisConn :: Redis.Connection
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
  withRunInIO $ \runInIO ->
  Redis.withCheckedConnect redisConnectInfo $ \conn -> 
  runInIO $ runStdoutLoggingT $ withPostgresqlPool (dbConnectionString config) 1 $ \pool ->
    action $ Env 
      { envPool = pool
      , envDebug = Maybe.fromMaybe False $ configDebug config
      , redisConn = conn
      }

  where 
    redisConnectInfo = Redis.defaultConnectInfo
      { Redis.connectHost = redisConfigHost $ configRedis config
      , Redis.connectPort = Redis.PortNumber . fromIntegral . redisConfigPort . configRedis $ config
      , Redis.connectMaxConnections = redisConfigMaxConnections $ configRedis config
      }

