{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumResponse (ForumResponse(..), DBModel, fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import Web.Obfuscate (Obfuscateable(..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Database.Esqueleto (Entity(..))

import DB.Model.User
import DB.Model.Forum
import Domain.Types.ForumAdministrator (ForumAdministrator)
import qualified Domain.Types.ForumAdministrator as ForumAdministrator

data ForumResponse = ForumResponse
  { frForumId :: ForumId
  , frName :: Text
  , frDescription :: Text
  , frCreator :: ForumAdministrator
  , frAdministrators :: [ForumAdministrator]
  }

data ObfuscatedForumResponse = ObfuscatedForumResponse 
  { ofrForumId :: Obfuscated ForumId
  , ofrName :: Text
  , ofrDescription :: Text
  , ofrCreator :: Obfuscated ForumAdministrator
  , ofrAdministrators :: Obfuscated [ForumAdministrator]
  }

instance Obfuscateable ForumResponse where
  type Obfuscated ForumResponse = ObfuscatedForumResponse

  obfuscate ctx response = ObfuscatedForumResponse
    { ofrForumId = obfuscate ctx $ frForumId response
    , ofrName = frName response 
    , ofrDescription = frDescription response
    , ofrCreator = obfuscate ctx $ frCreator response
    , ofrAdministrators = obfuscate ctx $ frAdministrators response 
    }

  deobfuscate ctx response = do
    forumId <- deobfuscate ctx $ ofrForumId response
    creator <- deobfuscate ctx $ ofrCreator response
    admins <- deobfuscate ctx $ ofrAdministrators response 
    pure $ ForumResponse
      { frForumId = forumId
      , frName = ofrName response 
      , frDescription = ofrDescription response
      , frCreator = creator
      , frAdministrators = admins
      }

type DBModel = (Entity Forum, Entity User, [Entity User]) 

fromModel :: DBModel -> ForumResponse
fromModel (Entity forumId forum, creator, admins) =
  ForumResponse
    { frForumId = forumId
    , frName = forumName forum
    , frDescription = forumDescription forum
    , frCreator = ForumAdministrator.fromModel creator
    , frAdministrators = fmap ForumAdministrator.fromModel admins
    }

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofr")} 'ObfuscatedForumResponse)
