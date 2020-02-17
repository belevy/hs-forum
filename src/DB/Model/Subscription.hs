{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.Subscription where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Time.Clock
import DB.Model.User
import DB.Model.Forum

share [mkPersist sqlSettings] [persistLowerCase|
  Subscription sql=subscriptions
    userId UserId
    forumId ForumId 
    createdAt UTCTime
    deriving Show
|]
