{-# LANGUAGE DeriveAnyClass #-}
module DB.User where

import           Control.Exception           (Exception)
import           Control.Monad.Except
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Text                   (Text)
import           Data.Time.Clock
import           Database.Esqueleto.Extended

import           DB.Model.User
import           Data.UserCredentials

data UserRegistrationError
  = UserAlreadyExists
  | BcryptFailure
  | DatabaseFailed
  deriving Show
  deriving anyclass Exception

registerUser :: MonadIO m => UserCredentials -> SqlPersistT m (Either UserRegistrationError (Entity User))
registerUser credentials = runExceptT $ do
  mUser <- lift $ findUserByUsername (ucUserName credentials)
  _ <- throwErrorIfExists UserAlreadyExists mUser
  newUser <- maybeToError BcryptFailure =<< credentialsToModel credentials
  lift $ insertEntity newUser
    where
      maybeToError err =
        maybe (throwError err) pure

      throwErrorIfExists err =
        maybe (pure ()) (\_ -> throwError err)


findUserByUsername :: MonadIO m => Text -> SqlReadT m (Maybe (Entity User))
findUserByUsername userName =
    selectFirst $ do
    users <- from $ table @User
    where_ $ users ^. UserUserName ==. val userName
    pure users

findUserByUserId :: MonadIO m => UserId -> SqlReadT m (Maybe (Entity User))
findUserByUserId userId =
    selectFirst $ do
    users <- from $ table @User
    where_ $ users ^. UserId ==. val userId
    pure users

findAndVerifyUser :: MonadIO m => UserCredentials -> SqlPersistT m (Maybe (Entity User))
findAndVerifyUser credentials = do
  mUser <- findUserByUsername (ucUserName credentials)
  pure $ verifiedUser =<< mUser

  where
    verifiedUser user =
      if credentialsValid credentials (entityVal user) then
        Just user
      else
        Nothing
