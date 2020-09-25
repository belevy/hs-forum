module UseCase.Forum.ListForumPosts where

import Control.Monad.Reader (MonadReader)
import UnliftIO (MonadUnliftIO)
import Env

import Data.Int
import qualified Data.Maybe as Maybe
import Data.PaginatedResponse
import Domain.Types.ForumPostResponse as ForumPostResponse
import Domain.Types.SessionData

import DB.Forum
import DB.Model.Forum

data Config = Config
  { configForumId :: ForumId
  , configSession :: SessionData
  , configPageSize :: Maybe Int64
  , configPageNumber :: Maybe Int64
  }

execute :: (MonadReader Env m, MonadUnliftIO m) => Config -> m (PaginatedResponse ForumPostResponse)
execute config = do
  fmap
    (toPaginatedResponse pageSize page ForumPostResponse.fromModel)
    (runDBReadOnly $ getTopPostsInForum (configForumId config) pageSize page)
  where
    page = Maybe.fromMaybe 1 $ configPageNumber config
    pageSize = Maybe.fromMaybe 25 $ configPageSize config
