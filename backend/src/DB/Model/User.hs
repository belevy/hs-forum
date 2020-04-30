{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.User where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.ByteString
import Data.Time.Clock
import Data.Aeson

share [mkPersist sqlSettings] [persistLowerCase|
  User json sql=users
    userName Text
    password Text
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deriving Show
|]
