module Control.Exception.Extended
  ( module E
  , mapException
  )
  where

import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception as E

mapException :: (MonadUnliftIO m, Exception e1, Exception e2)
             => (e1 -> e2)
             -> m a
             -> m a
mapException fn = handle (throwIO . fn)
