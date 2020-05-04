{-# LANGUAGE TemplateHaskell #-}
module Data.UserCredentials where 

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import Crypto.BCrypt

import DB.Model.User

data UserCredentials = UserCredentials
  { ucUserName :: Text
  , ucPassword :: Text
  } deriving Show

credentialsValid :: UserCredentials -> User -> Bool
credentialsValid uc user =
  validatePassword 
      (T.encodeUtf8 $ userPassword user) 
      (T.encodeUtf8 $ ucPassword uc)

credentialsToModel :: MonadIO m => UserCredentials -> m (Maybe User)
credentialsToModel cred = do
  mHashedPass <- liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (T.encodeUtf8 $ ucPassword cred)
  now <- liftIO  getCurrentTime
  pure $ flip fmap mHashedPass $ \hashedPass ->
    User
      { userUserName = ucUserName cred
      , userPassword = T.decodeUtf8 hashedPass
      , userCreatedAt = now
      , userUpdatedAt = Nothing
      }

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "uc")} 'UserCredentials)
