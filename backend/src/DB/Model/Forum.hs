{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.Forum where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Time.Clock

import DB.Model.User

share [mkPersist sqlSettings] [persistLowerCase|
  Forum sql=forums
    name Text
    description Text
    creator UserId
    createdAt UTCTime
    updatedAt UTCTime Maybe
    deletedAt UTCTime Maybe
    deriving Show
|]
