{-# LANGUAGE TemplateHaskell #-}
module Config where

import Data.Aeson
import Data.Aeson.TH
import Data.Yaml.Config
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import Data.String

data Config = Config 
  { configDb :: DatabaseConfig
  , configPort :: Int
  , configHashidsSalt :: String
  , configRedis :: RedisConfig
  , configDebug :: Maybe Bool
  } deriving Show

data DatabaseConfig = DatabaseConfig
  { dbConfigUser :: String
  , dbConfigPassword :: Maybe String
  , dbConfigHost :: String
  , dbConfigPort :: Int
  , dbConfigDbName :: String
  } deriving Show

data RedisConfig = RedisConfig
  { redisConfigHost :: String
  , redisConfigPort :: Int
  , redisConfigMaxConnections :: Int
  } deriving Show

dbConnectionString :: Config -> BS.ByteString
dbConnectionString cfg =
  fromString $ "postgresql://" 
         <> (dbConfigUser . configDb $ cfg)
         <> (maybe "" (":" <>) (dbConfigPassword . configDb $ cfg))
         <> "@"
         <> (dbConfigHost . configDb $ cfg)
         <> (":" <> (show . dbConfigPort . configDb $ cfg))
         <> ("/" <> (dbConfigDbName . configDb $ cfg))

loadConfigFile :: IO Config
loadConfigFile =
  loadYamlSettings ["config.yaml"] [] useEnv

$(deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '-' . drop (Text.length "config")} ''Config)
$(deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '-' . drop (Text.length "dbConfig")} ''DatabaseConfig)
$(deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '-' . drop (Text.length "redisConfig")} ''RedisConfig)
