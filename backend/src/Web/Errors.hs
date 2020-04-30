module Web.Errors
  where
    
import Servant (ServerError(..), err404, err401)
import UnliftIO (MonadIO, throwIO)

maybeNotFound :: (MonadIO m) => Maybe a -> m a
maybeNotFound = maybeThrowError err404{errBody="Could not find requested resource"}

maybeUnauthorized :: (MonadIO m) => Maybe a -> m a
maybeUnauthorized = maybeThrowError err401

maybeThrowError :: (MonadIO m) => ServerError -> Maybe a -> m a
maybeThrowError err = maybe (throwIO err) pure
