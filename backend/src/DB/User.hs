module DB.User where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except
import Database.Esqueleto.Extended
import Data.Text (Text)
import Data.Time.Clock

import Data.UserCredentials 
import DB.Model.User

data UserRegistrationError 
  = UserAlreadyExists
  | BcryptFailure
  | DatabaseFailed

registerUser :: MonadIO m => UserCredentials -> SqlPersistT m (Either UserRegistrationError (Entity User))
registerUser credentials = runExceptT $ do
  mUser <- lift $ fetchUserByUsername (ucUserName credentials)
  _ <- throwErrorIfExists UserAlreadyExists mUser
  newUser <- maybeToError BcryptFailure =<< credentialsToModel credentials
  lift $ insertEntity newUser
    where
      maybeToError err =
        maybe (throwError err) pure

      throwErrorIfExists err =
        maybe (pure ()) (\_ -> throwError err)


fetchUserByUsername :: MonadIO m => Text -> SqlReadT m (Maybe (Entity User))
fetchUserByUsername userName = 
    selectFirst $ do 
    users <- from $ Table @User
    where_ $ users ^. UserUserName ==. val userName
    pure users
  
fetchUserByUserId :: MonadIO m => UserId -> SqlReadT m (Maybe (Entity User))
fetchUserByUserId userId =
    selectFirst $ do 
    users <- from $ Table @User
    where_ $ users ^. UserId ==. val userId
    pure users

fetchAndVerifyUser :: MonadIO m => UserCredentials -> SqlPersistT m (Maybe (Entity User))
fetchAndVerifyUser credentials = do
  mUser <- fetchUserByUsername (ucUserName credentials)
  pure $ verifiedUser =<< mUser

  where
    verifiedUser user =
      if credentialsValid credentials (entityVal user) then
        Just user
      else
        Nothing
