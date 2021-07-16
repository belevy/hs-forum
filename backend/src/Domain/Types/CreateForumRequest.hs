{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.CreateForumRequest
  where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Text        as T

import           Web.Obfuscate
import           Web.Obfuscate.TH

import           DB.Model.User

data CreateForumRequest = CreateForumRequest
  { cfrName           :: T.Text
  , cfrDescription    :: T.Text
  , cfrAdministrators :: [UserId]
  }

$(deriveObfuscate defaultObfuscationOptions ''CreateForumRequest)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "obcfr")} 'ObfuscatedCreateForumRequest)
