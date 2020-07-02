{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumAdministrator (ForumAdministrator(..), fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import DB.Model.User
import Web.Obfuscate (Obfuscateable(..))
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto (Entity(..), Value(..))
    
data ForumAdministrator = ForumAdministrator
  { faUserId :: UserId
  , faUserName :: Text
  }

data ObfuscatedForumAdministrator = ObfuscatedForumAdministrator
  { ofaUserId :: Obfuscated UserId
  , ofaUserName :: Text
  }

instance Obfuscateable ForumAdministrator where
  type Obfuscated ForumAdministrator = ObfuscatedForumAdministrator
  obfuscate ctx response = ObfuscatedForumAdministrator
    { ofaUserId = obfuscate ctx $ faUserId response
    , ofaUserName = faUserName response
    }
  deobfuscate ctx response = do 
    userId <- deobfuscate ctx $ ofaUserId response
    pure $ ForumAdministrator
      { faUserId = userId
      , faUserName = ofaUserName response
      }

fromModel :: Entity User -> ForumAdministrator
fromModel (Entity userId user) =
  ForumAdministrator
    { faUserId = userId
    , faUserName = userUserName user
    }

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofa")} 'ObfuscatedForumAdministrator)
