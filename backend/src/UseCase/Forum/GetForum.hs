module UseCase.Forum.GetForum where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Fail (MonadFail)
import UnliftIO (MonadUnliftIO)
import Env

import qualified Data.Maybe as Maybe
import Data.PaginatedResponse
import Domain.Types.ForumResponse as ForumResponse
import Domain.Types.SessionData

import DB.Model.Forum
import DB.Forum

execute :: (MonadReader Env m, MonadUnliftIO m) 
        => SessionData 
        -> ForumId 
        -> m (Maybe ForumResponse)
execute _ forumId = do
  mForum <- runDBReadOnly $ getForumById forumId
  pure $ fmap ForumResponse.fromModel mForum
