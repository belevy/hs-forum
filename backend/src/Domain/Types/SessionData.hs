{-# LANGUAGE TemplateHaskell #-}
module Domain.Types.SessionData where

import Data.Aeson as A
import Data.Aeson.TH
import DB.Model.User
import qualified Data.Text as T
import qualified Data.ByteString as BS 
import qualified Data.ByteString.Lazy as LBS 
import Web.Obfuscate

data SessionData = SessionData 
  { sessionUser :: User }

data SessionResponse = SessionResponse
  { srUserName :: T.Text
  }

fromModel :: SessionData -> SessionResponse
fromModel session = SessionResponse
  { srUserName = userUserName $ sessionUser session
  }

encode :: SessionData -> BS.ByteString
encode = LBS.toStrict . A.encode 

decode :: BS.ByteString -> Maybe SessionData
decode = A.decodeStrict

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "session")} 'SessionData)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "sr")} 'SessionResponse)
