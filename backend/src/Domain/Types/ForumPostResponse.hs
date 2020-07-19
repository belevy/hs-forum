{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumPostResponse (ForumPostResponse(..), DBModel, fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import Web.Obfuscate 
import Web.Obfuscate.TH
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Database.Esqueleto as E (Entity(..), Value(..)) 

import DB.Model.ForumPost
import DB.Model.User

data ForumPostResponse = ForumPostResponse
  { fprForumPostId :: ForumPostId
  , fprContent :: Text
  , fprSortOrder :: Int
  , fprCreated :: UTCTime
  }

type DBModel = (Entity ForumPost, Entity User, E.Value Int) 

fromModel :: DBModel -> ForumPostResponse
fromModel (post, author, votes) =
  ForumPostResponse
    { fprForumPostId = entityKey post
    , fprContent = forumPostContent $ entityVal post
    , fprSortOrder = unValue votes
    , fprCreated = forumPostCreatedAt $ entityVal post
    }

$(deriveObfuscate defaultObfuscationOptions ''ForumPostResponse)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "obfpr")} 'ObfuscatedForumPostResponse)
