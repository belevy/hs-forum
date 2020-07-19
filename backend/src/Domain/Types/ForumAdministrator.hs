{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ForumAdministrator (ForumAdministrator(..), fromModel)
  where

import Data.Aeson
import Data.Aeson.TH
import DB.Model.User
import Web.Obfuscate 
import Web.Obfuscate.TH
import Data.Text (Text)
import qualified Data.Text as T
import Database.Esqueleto (Entity(..), Value(..))
    
data ForumAdministrator = ForumAdministrator
  { faUserId :: UserId
  , faUserName :: Text
  }

fromModel :: Entity User -> ForumAdministrator
fromModel (Entity userId user) =
  ForumAdministrator
    { faUserId = userId
    , faUserName = userUserName user
    }

$(deriveObfuscate defaultObfuscationOptions ''ForumAdministrator)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "obfa")} 'ObfuscatedForumAdministrator)
