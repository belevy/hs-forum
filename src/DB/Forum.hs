module DB.Forum
  where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Maybe as Maybe
import Data.Int

import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental

import DB.Model.Forum
import DB.Model.ForumPost
import DB.Model.User
import DB.Model.UserRole
import DB.Model.RoleType
import DB.Model.Vote
import DB.QueryCombinators

type ForumResult = (Entity Forum, Entity User, [Entity User])

getAllForums :: MonadIO m => Int64 -> Int64 -> SqlReadT m (Value Int64, [ForumResult])
getAllForums pageSize page = do
  totalCount <- select $ rowCount allForums
  forums <- select $ paginated pageSize page allForums
  forumsWithAdmins <- forM forums $ \(forum, creator) -> do
    admins <- getForumAdmins (entityKey forum)
    pure (forum, creator, admins)
  pure (head totalCount, forumsWithAdmins)

getForumAdmins :: MonadIO m => ForumId -> SqlReadT m [Entity User]
getForumAdmins forumId =
  select $ do
    (users :& roles) <-
      from $ Table @User
      `InnerJoin` Table @UserRole
      `on` (\(users :& roles) ->
            users ^. UserId ==. roles ^. UserRoleUserId)
    where_ $ roles ^. UserRoleRoleType ==. val Administrator
        &&. roles ^. UserRoleForumId ==. val forumId
    pure users

getForumById :: MonadIO m => ForumId -> SqlReadT m (Maybe ForumResult)
getForumById forumId = do
  mForum <- fmap Maybe.listToMaybe $ select $ do
    (forums, users) <- allForums
    where_ $ forums ^. ForumId ==. val forumId
    pure (forums, users)
  admins <- getForumAdmins forumId
  pure $ mForum >>= (\(forum,creator) -> pure (forum, creator, admins))

getTopPostsInForum :: MonadIO m => ForumId -> Int64 -> Int64 -> SqlReadT m (Value Int64, [(Entity ForumPost, Entity User, Value Int)])
getTopPostsInForum forumId pageSize page = do
  totalCount <- select . rowCount . from $ SelectQuery forumPosts
  paginatedResults <- select $ paginated pageSize page $ forumPosts
  pure (head totalCount, paginatedResults)
    where
      forumPosts = do
        res@(_, post, u, v) <- topPostsQuery
        where_ $ post ^. ForumPostForumId ==. val forumId
        pure (post, u, v)

allForums :: SqlQuery (SqlExpr (Entity Forum), SqlExpr (Entity User))
allForums = do
  (forums :& users) <-
    from $ Table @Forum
    `InnerJoin` Table @User
    `on` (\(forums :& users) ->
          forums ^. ForumCreator ==. users ^. UserId)
  pure (forums, users)

topPostsQuery :: SqlQuery (SqlExpr (Entity Forum), SqlExpr (Entity ForumPost), SqlExpr (Entity User), SqlExpr (Value Int))
topPostsQuery = do
  (forum :& posts :& users :& votes) <-
    from $ Table @Forum
    `InnerJoin` Table @ForumPost
    `on` (\(forum :& posts) ->
        forum ^. ForumId ==. posts ^. ForumPostForumId)
    `InnerJoin` Table @User
    `on` (\(_ :& posts :& users) ->
          posts ^. ForumPostAuthorId ==. users ^. UserId)
    `LeftOuterJoin` Table @Vote
    `on` (\(_ :& posts :& _ :& votes) ->
            just (posts ^. ForumPostId) ==. votes ?. VotePostId)
  groupBy (posts ^. ForumPostId, users ^. UserId)
  let voteTotal = coalesceDefault [sum_ (votes ?. VoteValue)] (val 0)
  orderBy [desc voteTotal]
  pure (forum, posts, users, voteTotal)
