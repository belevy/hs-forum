{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumResponse (ForumResponse(..), DBModel, fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import Web.Obfuscate 
import Web.Obfuscate.TH
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

$(deriveObfuscate defaultObfuscationOptions ''ForumResponse)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "obfr")} 'ObfuscatedForumResponse)
