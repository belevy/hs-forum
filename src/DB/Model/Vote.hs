{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.Vote where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Data.Text
import Data.Time.Clock

import DB.Model.User
import DB.Model.ForumPost

share [mkPersist sqlSettings] [persistLowerCase|
  Vote sql=votes
    voterId UserId
    forumPostId ForumPostId
    value Int 
    createdAt UTCTime
    deletedAt UTCTime Maybe
    deriving Show
|]
