{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  where

import Servant
import Data.Aeson
import Data.Aeson.TH
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

import DB.Forum
import DB.Model.Forum
import Domain.Types.SessionData as SessionData
import Domain.Types.ForumAdministrator as ForumAdministrator
import Domain.Types.ForumPostResponse as ForumPostResponse
import Domain.Types.ForumResponse as ForumResponse

type Api = "forums" :> (ListForums :<|> GetForum :<|> GetForumPosts)

type ListForums
        = Protected
        :> QueryParam "pageSize" Int64
        :> QueryParam "page" Int64
        :> Obfuscate :> Get '[JSON] (PaginatedResponse ForumResponse)

type GetForum
        = Protected
        :> Obfuscate :> Capture "forumId" ForumId
        :> Obfuscate :> Get '[JSON] ForumResponse

type GetForumPosts
        = Protected
        :> Obfuscate :> Capture "forumId" ForumId
        :> "posts"
        :> QueryParam "pageSize" Int64
        :> QueryParam "page" Int64
        :> Obfuscate :> Get '[JSON] (PaginatedResponse ForumPostResponse)

server :: AppServer Api
server = listForums :<|> getForum :<|> getForumPosts
  where
    listForums :: SessionData
               -> Maybe Int64
               -> Maybe Int64
               -> AppHandler (PaginatedResponse ForumResponse)
    listForums _ mPageSize mPage =
      fmap
      (toPaginatedResponse pageSize page ForumResponse.fromModel)
      (runDBReadOnly $ getAllForums pageSize page)

      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize

    getForum :: SessionData -> ForumId -> AppHandler ForumResponse
    getForum _ forumId = do
      mForum <- runDBReadOnly $ getForumById forumId
      maybeNotFound $ fmap ForumResponse.fromModel mForum

    getForumPosts :: SessionData
                  -> ForumId
                  -> Maybe Int64
                  -> Maybe Int64
                  -> AppHandler (PaginatedResponse ForumPostResponse)
    getForumPosts _ forumId mPageSize mPage =
      fmap
      (toPaginatedResponse pageSize page ForumPostResponse.fromModel)
      (runDBReadOnly $ getTopPostsInForum forumId pageSize page)
      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize

