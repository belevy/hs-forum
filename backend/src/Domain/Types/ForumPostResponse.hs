{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumPostResponse (ForumPostResponse(..), DBModel, fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import Web.Obfuscate (Obfuscateable(..))
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

data ObfuscatedForumPostResponse = ObfuscatedForumPostResponse
  { ofprForumPostId :: Obfuscated ForumPostId
  , ofprContent :: Text
  , ofprSortOrder :: Int
  , ofprCreated :: UTCTime
  }

instance Obfuscateable ForumPostResponse where
  type Obfuscated ForumPostResponse = ObfuscatedForumPostResponse
  obfuscate ctx response = ObfuscatedForumPostResponse
      { ofprForumPostId = obfuscate ctx $ fprForumPostId response
      , ofprContent = fprContent response 
      , ofprSortOrder = fprSortOrder response 
      , ofprCreated = fprCreated response 
      }
  deobfuscate ctx response = do 
    forumPostId <- deobfuscate ctx $ ofprForumPostId response
    pure $ ForumPostResponse
      { fprForumPostId= forumPostId 
      , fprContent = ofprContent response 
      , fprSortOrder = ofprSortOrder response 
      , fprCreated = ofprCreated response 
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
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofpr")} 'ObfuscatedForumPostResponse)
