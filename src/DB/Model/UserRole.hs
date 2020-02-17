{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module DB.Model.UserRole where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import DB.Model.User
import DB.Model.RoleType
import DB.Model.Forum

share [mkPersist sqlSettings] [persistLowerCase|
  UserRole sql=user_roles
    userId UserId
    roleType RoleType
    forumId ForumId
    deriving Show
|]
