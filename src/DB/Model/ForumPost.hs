{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.ForumPost where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Time.Clock
import DB.Model.Forum

share [mkPersist sqlSettings] [persistLowerCase|
  ForumPost sql=forum_posts
    forumId ForumId
    content Text
    createdAt UTCTime
    deletedAt UTCTime Maybe
    modifiedFrom ForumPostId Maybe
    deriving Show
|]
