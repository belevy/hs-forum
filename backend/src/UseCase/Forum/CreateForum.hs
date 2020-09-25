module UseCase.Forum.CreateForum
  where

import Control.Monad.Reader (MonadReader)
import Control.Monad.Fail (MonadFail)
import UnliftIO (MonadUnliftIO)
import Env
import Domain.Types.ForumResponse as ForumResponse
import Domain.Types.SessionData
import Domain.Types.CreateForumRequest
import DB.Forum as DB (CreateForumData(..), createForum, getForumById)

execute :: (MonadReader Env m, MonadFail m, MonadUnliftIO m) => SessionData -> CreateForumRequest -> m ForumResponse
execute session request = do
    Just forum <- runDB $ do
      forumId <- DB.createForum $ CreateForumData
          { cfdUser = sessionUser session
          , cfdForumName = cfrName request
          , cfdForumDescription = cfrDescription request
          , cfdForumAdministrators = cfrAdministrators request
          }
      getForumById forumId
    pure $ ForumResponse.fromModel forum
