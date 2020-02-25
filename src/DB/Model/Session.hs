module DB.Model.Session where

import Database.Persist.Sql
import Data.ByteString
import Text.Read
import DB.Model.User 
import qualified Data.ByteString.Char8 as C8

type SessionKey = ByteString
type Session = User

encode :: Session -> ByteString
encode = encode

decode :: ByteString -> Maybe Session
decode = decode
