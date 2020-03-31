module DB.QueryCombinators (paginated, rowCount)
  where

import Control.Monad
import Database.Esqueleto
import Data.Int

paginated :: Int64 -> Int64 -> SqlQuery a -> SqlQuery a
paginated pageSize page q = do
  res <- q
  limit pageSize
  offset ((page - 1) * pageSize)
  pure res

rowCount :: SqlQuery a -> SqlQuery (SqlExpr (Value Int64))
rowCount q = do
  void q
  pure countRows
