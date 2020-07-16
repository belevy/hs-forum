{-# LANGUAGE TemplateHaskell #-} 
module Data.PaginatedResponse 
    ( PaginatedResponse
    , toPaginatedResponse
    ) where

import Data.Aeson (defaultOptions, fieldLabelModifier, camelTo2)
import Data.Aeson.TH (deriveToJSON)
import qualified Data.Text as T
import Data.Int
import Web.Obfuscate

import Env
  
data PaginatedResponse a = PaginatedResponse
  { paginatedTotalCount :: Int64
  , paginatedCurrentPage :: Int64
  , paginatedPageSize :: Int64
  , paginatedData :: [a]
  }

type instance Obfuscated (PaginatedResponse a) = PaginatedResponse (Obfuscated a)
instance CanObfuscate a => CanObfuscate (PaginatedResponse a) where
  obfuscate ctx response = PaginatedResponse
    { paginatedTotalCount = paginatedTotalCount response
    , paginatedCurrentPage = paginatedCurrentPage response
    , paginatedPageSize = paginatedPageSize response
    , paginatedData = obfuscate ctx $ paginatedData response
    }
instance CanDeobfuscate a => CanDeobfuscate (PaginatedResponse a) where
  deobfuscate ctx response = do
    deobfuscatedData <- deobfuscate ctx $ paginatedData response
    pure $ PaginatedResponse 
      { paginatedTotalCount = paginatedTotalCount response
      , paginatedCurrentPage = paginatedCurrentPage response
      , paginatedPageSize = paginatedPageSize response
      , paginatedData = deobfuscatedData
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
