{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  where

import Servant
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as LC8
import Data.Time.Clock (UTCTime)
import UnliftIO (MonadUnliftIO, liftIO, throwIO, handle, catch)
import Control.Exception (SomeException(..), mapException)
import qualified Data.Maybe as Maybe
import Data.Int

import Web.Obfuscate
import Web.AppHandler
import Web.Errors
import Web.Auth
import Web.Servant.Csrf

import Data.PaginatedResponse
import Env

import DB.Forum as DB
import DB.Model.Forum
import Domain.Types.SessionData as SessionData
import Domain.Types.ForumAdministrator as ForumAdministrator
import Domain.Types.ForumPostResponse as ForumPostResponse
import Domain.Types.ForumResponse as ForumResponse
import Domain.Types.CreateForumRequest as CreateForumRequest

type Api = "forums" :> (ListForums :<|> GetForum :<|> GetForumPosts :<|> CreateForum)

type ListForums
        = Protected
        :> QueryParam "pageSize" Int64
        :> QueryParam "page" Int64
        :> Obfuscate :> Get '[JSON] (WithCSRFToken (PaginatedResponse ForumResponse))

type GetForum
        = Protected
        :> Obfuscate :> Capture "forumId" ForumId
        :> Obfuscate :> Get '[JSON] (WithCSRFToken ForumResponse)

type GetForumPosts
        = Protected
        :> Obfuscate :> Capture "forumId" ForumId
        :> "posts"
        :> QueryParam "pageSize" Int64
        :> QueryParam "page" Int64
        :> Obfuscate :> Get '[JSON] (WithCSRFToken (PaginatedResponse ForumPostResponse))

type CreateForum
        = Protected
        :> CheckCSRF
        :> Obfuscate :> ReqBody '[JSON] CreateForumRequest
        :> Obfuscate :> Post '[JSON] (WithCSRFToken ForumResponse)

server :: AppServer Api
server = listForums :<|> getForum :<|> getForumPosts :<|> createForum
  where
    listForums :: SessionData
               -> Maybe Int64
               -> Maybe Int64
               -> AppHandler (WithCSRFToken (PaginatedResponse ForumResponse))
    listForums _ mPageSize mPage = do
      resp <- fmap
        (toPaginatedResponse pageSize page ForumResponse.fromModel)
        (runDBReadOnly $ getAllForums pageSize page)
      addCsrfToken resp

      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize

    getForum :: SessionData -> ForumId -> AppHandler (WithCSRFToken ForumResponse)
    getForum _ forumId = do
      mForum <- runDBReadOnly $ getForumById forumId
      resp <- maybeNotFound $ fmap ForumResponse.fromModel mForum
      addCsrfToken resp

    getForumPosts :: SessionData
                  -> ForumId
                  -> Maybe Int64
                  -> Maybe Int64
                  -> AppHandler (WithCSRFToken (PaginatedResponse ForumPostResponse))
    getForumPosts _ forumId mPageSize mPage = do
      resp <- fmap
        (toPaginatedResponse pageSize page ForumPostResponse.fromModel)
        (runDBReadOnly $ getTopPostsInForum forumId pageSize page)
      addCsrfToken resp
      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize


    createForum :: SessionData -> CreateForumRequest -> AppHandler (WithCSRFToken ForumResponse)
    createForum session request = do
      Just forum <- runDB $ do
        forumId <- DB.createForum $ CreateForumData
            { cfdUser = sessionUser session
            , cfdForumName = cfrName request
            , cfdForumDescription = cfrDescription request
            , cfdForumAdministrators = cfrAdministrators request
            }
        getForumById forumId
      addCsrfToken $ ForumResponse.fromModel forum
