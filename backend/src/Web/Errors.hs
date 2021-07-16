module Web.Errors
  where

import           Network.HTTP.Types (forbidden403, notFound404, unauthorized401)
import           UnliftIO           (MonadIO, throwIO)
import           Web.Eved.Server    (ServerError (..))


maybeNotFound :: (MonadIO m) => Maybe a -> m a
maybeNotFound = maybeThrowError err404{errorBody="Could not find requested resource"}

maybeUnauthorized :: (MonadIO m) => Maybe a -> m a
maybeUnauthorized = maybeThrowError err401

maybeThrowError :: (MonadIO m) => ServerError -> Maybe a -> m a
maybeThrowError err = maybe (throwIO err) pure

err404 :: ServerError
err404 =
    ServerError
        { errorStatus = notFound404
        , errorHeaders = []
        , errorBody = "Not Found"
        }

err401 :: ServerError
err401 =
    ServerError
        { errorStatus = unauthorized401
        , errorHeaders = []
        , errorBody = "Unauthorized"
        }

err403 :: ServerError
err403 =
    ServerError
        { errorStatus = forbidden403
        , errorHeaders = []
        , errorBody = "Forbidden"
        }
