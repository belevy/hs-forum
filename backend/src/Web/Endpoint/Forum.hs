{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  ( Api
  , server
  ) where

import Servant
import Data.Aeson
import Data.Aeson.TH
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import UnliftIO (liftIO, throwIO)
import qualified Data.Maybe as Maybe
import Data.Int

import Web.Obfuscate
import Web.AppHandler
import Web.Errors
import Web.Auth

import Data.PaginatedResponse
import Env

import Database.Esqueleto as E (Entity(..), Value(..))
import DB.Forum
import DB.Model.Forum
import DB.Model.ForumPost
import DB.Model.User

type Api = Protected :> "forums" :> 
      (    QueryParam "pageSize" Int64
           :> QueryParam "page" Int64
           :> ObfuscatedGet '[JSON] (PaginatedResponse ForumResponse)
      :<|> ObfuscatedCapture "forumId" ForumId :> ObfuscatedGet '[JSON] ForumResponse
      :<|> ObfuscatedCapture "forumId" ForumId
           :> "posts"
           :> QueryParam "pageSize" Int64
           :> QueryParam "page" Int64
           :> ObfuscatedGet '[JSON] (PaginatedResponse ForumPostResponse)
      )

data ForumResponse = ForumResponse
  { frForumId :: ForumId
  , frName :: Text
  , frDescription :: Text
  , frCreator :: ForumAdministrator
  , frAdministrators :: [ForumAdministrator]
  }

data ForumAdministrator = ForumAdministrator
  { faUserId :: UserId
  , faUserName :: Text
  }

data ForumPostResponse = ForumPostResponse
  { fprForumPostId :: ForumPostId
  , fprContent :: Text
  , fprSortOrder :: Int
  , fprCreated :: UTCTime
  }

server :: AppServer Api
server _session = listForums :<|> getForum  :<|> getForumPosts
  where
    listForums :: Maybe Int64
               -> Maybe Int64
               -> AppHandler (PaginatedResponse ForumResponse)
    listForums mPageSize mPage =
      fmap
      (toPaginatedResponse pageSize page forumToResponse)
      (runDBReadOnly $ getAllForums pageSize page)

      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize

    getForum :: ForumId -> AppHandler ForumResponse
    getForum forumId = do
      mForum <- runDBReadOnly $ getForumById forumId
      maybeNotFound $ fmap forumToResponse mForum

    getForumPosts :: ForumId
                  -> Maybe Int64
                  -> Maybe Int64
                  -> AppHandler (PaginatedResponse ForumPostResponse)
    getForumPosts forumId mPageSize mPage =
      fmap
      (toPaginatedResponse pageSize page packResponse)
      (runDBReadOnly $ getTopPostsInForum forumId pageSize page)
      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize
        packResponse (post, author, votes) =
          ForumPostResponse
            { fprForumPostId = entityKey post
            , fprContent = forumPostContent $ entityVal post
            , fprSortOrder = unValue votes
            , fprCreated = forumPostCreatedAt $ entityVal post
            }

userToForumAdministrator :: Entity User -> ForumAdministrator
userToForumAdministrator (Entity userId user) =
  ForumAdministrator
    { faUserId = userId
    , faUserName = userUserName user
    }

forumToResponse :: (Entity Forum, Entity User, [Entity User]) -> ForumResponse
forumToResponse (Entity forumId forum, creator, admins) =
  ForumResponse
    { frForumId = forumId
    , frName = forumName forum
    , frDescription = forumDescription forum
    , frCreator = userToForumAdministrator creator
    , frAdministrators = fmap userToForumAdministrator admins
    }

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fa")} 'ForumAdministrator)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fr")} 'ForumResponse)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fpr")} 'ForumPostResponse)
