module UseCase.Forum.ListForums where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Fail (MonadFail)
import UnliftIO (MonadUnliftIO)
import Env

import Data.Int
import qualified Data.Maybe as Maybe
import Data.PaginatedResponse
import Domain.Types.ForumResponse as ForumResponse
import Domain.Types.SessionData

import DB.Forum

data Config = Config
  { configSession :: SessionData
  , configPageSize :: Maybe Int64
  , configPageNumber :: Maybe Int64
  }

execute :: (MonadReader Env m, MonadUnliftIO m) => Config -> m (PaginatedResponse ForumResponse)
execute config = do
  let page = Maybe.fromMaybe 1 $ configPageNumber config
      pageSize = Maybe.fromMaybe 25 $ configPageSize config
  forums <- runDBReadOnly $ getAllForums pageSize page
  pure $ toPaginatedResponse pageSize page ForumResponse.fromModel forums
