module Lib
    ( someFunc
    ) where

import Control.Monad.IO.Class
import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental
import DB.Model.User
import DB.User
import Control.Monad.Logger (runNoLoggingT)
import Data.Time.Clock
import Data.UserCredentials
import Control.Concurrent

someFunc :: IO ()
someFunc =  do
  currentTime <- getCurrentTime
  pure ()
