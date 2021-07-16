{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE RecordWildCards    #-}

module DB.Forum
  where

import           Control.Exception.Extended
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Int
import qualified Data.Maybe                  as Maybe
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Database.Esqueleto.Extended
import           UnliftIO                    (Exception (..), MonadUnliftIO,
                                              SomeException (..), handle,
                                              throwIO)

import           DB.Model.Forum
import           DB.Model.ForumPost
import           DB.Model.RoleType
import           DB.Model.User
import           DB.Model.UserRole
import           DB.Model.Vote
import           DB.QueryCombinators
import           DB.User

type ForumResult = (Entity Forum, Entity User, [Entity User])

getAllForums :: MonadIO m => Int64 -> Int64 -> SqlReadT m (Int64, [ForumResult])
getAllForums pageSize page = do
  totalCount <- selectCount allForums
  forums <- select $ paginated pageSize page allForums
  forumsWithAdmins <- forM forums $ \(forum, creator) -> do
    admins <- getForumAdmins (entityKey forum)
    pure (forum, creator, admins)
  pure (totalCount, forumsWithAdmins)

getForumAdmins :: MonadIO m => ForumId -> SqlReadT m [Entity User]
getForumAdmins forumId =
  select $ do
    (users :& roles) <-
      from $ table @User
      `innerJoin` table @UserRole
      `on` (\(users :& roles) ->
            users ^. UserId ==. roles ^. UserRoleUserId)
    where_ $ roles ^. UserRoleRoleType ==. val Administrator
        &&. roles ^. UserRoleForumId ==. val forumId
    pure users

getForumById :: MonadIO m => ForumId -> SqlReadT m (Maybe ForumResult)
getForumById forumId = do
  mForum <- selectFirst $ do
    (forums, users) <- allForums
    where_ $ forums ^. #id ==. val forumId
    pure (forums, users)
  admins <- getForumAdmins forumId
  pure $ mForum >>= (\(forum,creator) -> pure (forum, creator, admins))

getTopPostsInForum :: MonadIO m => ForumId -> Int64 -> Int64 -> SqlReadT m (Int64, [(Entity ForumPost, Entity User, Value Int)])
getTopPostsInForum forumId pageSize page = do
  totalCount <- selectCount $ from forumPosts
  paginatedResults <- select $ paginated pageSize page $ forumPosts
  pure (totalCount, paginatedResults)
    where
      forumPosts = do
        res@(_, post, u, v) <- topPostsQuery
        where_ $ post ^. ForumPostForumId ==. val forumId
        pure (post, u, v)

allForums :: SqlQuery (SqlExpr (Entity Forum), SqlExpr (Entity User))
allForums = do
  (forums :& users) <-
    from $ table @Forum
    `innerJoin` table @User
    `on` (\(forums :& users) ->
          forums ^. ForumCreator ==. users ^. UserId)
  pure (forums, users)

topPostsQuery :: SqlQuery (SqlExpr (Entity Forum), SqlExpr (Entity ForumPost), SqlExpr (Entity User), SqlExpr (Value Int))
topPostsQuery = do
  (forum :& posts :& users :& votes) <-
    from $ table @Forum
    `innerJoin` table @ForumPost
    `on` (\(forum :& posts) ->
        forum ^. ForumId ==. posts ^. ForumPostForumId)
    `innerJoin` table @User
    `on` (\(_ :& posts :& users) ->
          posts ^. ForumPostAuthorId ==. users ^. UserId)
    `leftJoin` table @Vote
    `on` (\(_ :& posts :& _ :& votes) ->
            just (posts ^. ForumPostId) ==. votes ?. VotePostId)
  groupBy (posts ^. ForumPostId, users ^. UserId)
  let voteTotal = coalesceDefault [sum_ (votes ?. VoteValue)] (val 0)
  orderBy [desc voteTotal]
  pure (forum, posts, users, voteTotal)

data CreateForumData = CreateForumData
  { cfdUser                :: User
  , cfdForumName           :: T.Text
  , cfdForumDescription    :: T.Text
  , cfdForumAdministrators :: [UserId]
  }

data ForumCreationError
  = CreatorNotFound
  | FailedToInsertForum
  | FailedToInsertAdministrator
  deriving (Show)
  deriving anyclass (Exception)

createForum :: MonadUnliftIO m => CreateForumData -> SqlPersistT m ForumId
createForum CreateForumData{..} = do
  now <- liftIO getCurrentTime
  creator <- maybe (throwIO CreatorNotFound) pure =<< findUserByUsername (userUserName cfdUser)
  forumId <- insertOrThrow FailedToInsertForum $
    Forum
      { forumName = cfdForumName
      , forumDescription = cfdForumDescription
      , forumCreator = entityKey creator
      , forumCreatedAt = now
      , forumUpdatedAt = Nothing
      , forumDeletedAt = Nothing
      }
  forM_ cfdForumAdministrators $ \userId ->
    insertOrThrow FailedToInsertAdministrator $
      UserRole
        { userRoleUserId = userId
        , userRoleForumId = forumId
        , userRoleRoleType = Administrator
        }
  pure forumId

  where
    insertOrThrow err r =
      mapException (\(SomeException _) -> err) $ insert r
