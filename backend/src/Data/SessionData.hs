{-# LANGUAGE TemplateHaskell #-}
module Data.SessionData where

import Data.Aeson
import Data.Aeson.TH
import DB.Model.User
import qualified Data.Text as T

data SessionData = SessionData 
  { sessionUser :: User }

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "session")} 'SessionData)
