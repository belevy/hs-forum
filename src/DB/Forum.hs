module DB.Forum
  where

import Control.Monad.IO.Class
import qualified Data.Maybe as Maybe

import Database.Esqueleto hiding (from, on)
import Database.Esqueleto.Experimental

import DB.Model.Forum
import DB.Model.ForumPost
import DB.Model.User
import DB.Model.UserRole
import DB.Model.RoleType
import DB.Model.Vote

getForumById :: MonadIO m => ForumId -> SqlPersistT m (Maybe (Entity Forum, Entity User, [Entity User]))
getForumById forumId = do 
  mForum <- fmap Maybe.listToMaybe $ select $ do
    (forums :& users) <- 
      from $ Table @Forum 
      `InnerJoin` Table @User
      `on` (\(forums :& users) ->
            forums ^. ForumCreator ==. users ^. UserId)
    where_ $ forums ^. ForumId ==. val forumId
    pure (forums, users)
  admins <- select $ do
    (users :& roles) <- 
      from $ Table @User
      `InnerJoin` Table @UserRole 
      `on` (\(users :& roles) ->
            users ^. UserId ==. roles ^. UserRoleUserId)
    where_ $ roles ^. UserRoleRoleType ==. val Administrator
        &&. roles ^. UserRoleForumId ==. val forumId
    pure users
  
  pure $ mForum >>= (\(forum,creator) -> pure (forum, creator, admins))

getTopPosts :: MonadIO m => ForumId -> Int -> Int -> SqlPersistT m (Int, [(Entity ForumPost, Entity User, Value Int)])
getTopPosts forumId page pageSize = do
  totalCount <- fmap (unValue . head) $ select $ do
    postIds <- from (SelectQuery $ do
      (post, author, score) <- topPostsQuery
      where_ $ post ^. ForumPostForumId ==. val forumId
      pure $ post ^. ForumPostId)
    pure $ count postIds
  paginatedResults <- select $ do
    (post, author, score) <- topPostsQuery
    where_ $ post ^. ForumPostForumId ==. val forumId
    limit (fromIntegral pageSize)
    offset (fromIntegral ((page - 1) * pageSize))
    pure (post, author, score)

  pure (totalCount, paginatedResults)

topPostsQuery :: SqlQuery (SqlExpr (Entity ForumPost), SqlExpr (Entity User), SqlExpr (Value Int))
topPostsQuery = do
  (posts :& users :& votes) <- 
    from $ Table @ForumPost
    `InnerJoin` Table @User
    `on` (\(posts :& users) ->
          posts ^. ForumPostAuthorId ==. users ^. UserId)
    `LeftOuterJoin` Table @Vote
    `on` (\(posts :& _ :& votes) ->
            just (posts ^. ForumPostId) ==. votes ?. VotePostId)
  groupBy (posts ^. ForumPostId, users ^. UserId)
  let voteTotal = coalesceDefault [sum_ (votes ?. VoteValue)] (val 0)
  orderBy [desc voteTotal]
  pure (posts, users, voteTotal)
