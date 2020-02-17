{-# LANGUAGE TemplateHaskell #-}

module DB.Model.RoleType where 

import Database.Persist.TH

data RoleType 
  = Administrator 
  | Poster 
  | Reader
  deriving (Show, Read, Eq)
          

derivePersistField "RoleType"
