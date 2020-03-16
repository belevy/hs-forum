module Web.Errors
  where
    
import Servant (ServerError(..), err404)
import UnliftIO (MonadIO, throwIO)

maybeNotFound :: (MonadIO m) => Maybe a -> m a
maybeNotFound = maybe (throwIO err404{errBody="Could not find requested resource"}) pure
