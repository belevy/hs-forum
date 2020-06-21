{-# LANGUAGE TemplateHaskell #-}

module Data.PaginatedResponse 
    ( PaginatedResponse
    , toPaginatedResponse
    ) where

import Data.Aeson (defaultOptions, fieldLabelModifier, camelTo2)
import Data.Aeson.TH (deriveToJSON)
import qualified Data.Text as T
import Data.Int

import Env
  
data PaginatedResponse a = PaginatedResponse
  { paginatedTotalCount :: Int64
  , paginatedCurrentPage :: Int64
  , paginatedPageSize :: Int64
  , paginatedData :: [a]
  }

toPaginatedResponse :: Int64
                    -> Int64
                    -> (a -> b)
                    -> (Int64, [a])
                    -> PaginatedResponse b
toPaginatedResponse pageSize page packResponse (totalCount, results) =
  PaginatedResponse
    { paginatedTotalCount = totalCount
    , paginatedCurrentPage = page
    , paginatedPageSize = pageSize
    , paginatedData = fmap packResponse results
    }

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "paginated")} 'PaginatedResponse)
