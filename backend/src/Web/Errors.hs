module Web.Errors
  where

import           Network.HTTP.Types (conflict409, forbidden403,
                                     internalServerError500, notFound404,
                                     unauthorized401)
import           UnliftIO           (MonadIO, throwIO)
import           Web.Eved.Server    (ServerError (..))


maybeNotFound :: (MonadIO m) => Maybe a -> m a
maybeNotFound = maybeThrowError err404{errorBody="Could not find requested resource"}

maybeUnauthorized :: (MonadIO m) => Maybe a -> m a
maybeUnauthorized = maybeThrowError err401

maybeThrowError :: (MonadIO m) => ServerError -> Maybe a -> m a
maybeThrowError err = maybe (throwIO err) pure

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

err404 :: ServerError
err404 =
    ServerError
        { errorStatus = notFound404
        , errorHeaders = []
        , errorBody = "Not Found"
        }

err409 :: ServerError
err409 =
    ServerError
        { errorStatus = conflict409
        , errorHeaders = []
        , errorBody = "Conflict"
        }

err500 :: ServerError
err500 =
    ServerError
        { errorStatus = internalServerError500
        , errorHeaders = []
        , errorBody = "Internal Server Error"
        }
