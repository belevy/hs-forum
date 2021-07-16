{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module DB.Model.Vote where

import           Data.Text
import           Data.Time.Clock
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

import           DB.Model.ForumPost
import           DB.Model.User

share [mkPersist sqlSettings, mkMigrate "mirateVote"] [persistLowerCase|
  Vote sql=votes
    voterId UserId
    postId ForumPostId
    value Int
    createdAt UTCTime
    deletedAt UTCTime Maybe
    deriving Show
|]
