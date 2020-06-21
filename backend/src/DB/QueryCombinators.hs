module DB.QueryCombinators (paginated)
  where

import Control.Monad
import Database.Esqueleto.Extended
import Data.Int

paginated :: Int64 -> Int64 -> SqlQuery a -> SqlQuery a
paginated pageSize page q = do
  res <- q
  limit pageSize
  offset ((page - 1) * pageSize)
  pure res
