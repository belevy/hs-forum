{-# LANGUAGE TemplateHaskell #-}
module Data.SessionData where

import Data.Aeson as A
import Data.Aeson.TH
import DB.Model.User
import qualified Data.Text as T
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as LBS 

data SessionData = SessionData 
  { sessionUser :: User }

encode :: SessionData -> BS.ByteString
encode = LBS.toStrict . A.encode 

decode :: BS.ByteString -> Maybe SessionData
decode = A.decode . LBS.fromStrict

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "session")} 'SessionData)
