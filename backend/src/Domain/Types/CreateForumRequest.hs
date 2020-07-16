{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.CreateForumRequest 
  where

import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as T

import Web.Obfuscate

import DB.Model.User 

data CreateForumRequest = CreateForumRequest
  { cfrName :: T.Text
  , cfrDescription :: T.Text
  , cfrAdministrators :: [UserId]
  }

$(deriveObfuscate defaultObfuscationOptions ''CreateForumRequest)
$(deriveFromJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "obcfr")} 'ObfuscatedCreateForumRequest)
