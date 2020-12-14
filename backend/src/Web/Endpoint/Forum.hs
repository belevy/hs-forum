{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  where

import Servant
import Data.Int

import Web.Obfuscate
import Web.AppHandler
import Web.Errors
import Web.Auth
import Web.Servant.Csrf
import Web.Servant.Obfuscate

import Data.PaginatedResponse

import DB.Model.Forum
import Domain.Types.SessionData as SessionData
import Domain.Types.ForumPostResponse as ForumPostResponse
import Domain.Types.ForumResponse as ForumResponse
import Domain.Types.CreateForumRequest as CreateForumRequest

import qualified UseCase.Forum.CreateForum as CreateForum 
import qualified UseCase.Forum.ListForums as ListForums
import qualified UseCase.Forum.GetForum as GetForum
import qualified UseCase.Forum.ListForumPosts as ListForumPosts

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
    listForums session mPageSize mPage = do
      addCsrfToken =<< 
        ListForums.execute (ListForums.Config
          { ListForums.configSession = session
          , ListForums.configPageSize = mPageSize
          , ListForums.configPageNumber = mPage
          })

    getForum :: SessionData -> ForumId -> AppHandler (WithCSRFToken ForumResponse)
    getForum sessionData forumId = do
      GetForum.execute sessionData forumId
        >>= maybeNotFound
        >>= addCsrfToken

    getForumPosts :: SessionData
                  -> ForumId
                  -> Maybe Int64
                  -> Maybe Int64
                  -> AppHandler (WithCSRFToken (PaginatedResponse ForumPostResponse))
    getForumPosts session forumId mPageSize mPage = do
      addCsrfToken =<< 
        ListForumPosts.execute (ListForumPosts.Config
          { ListForumPosts.configForumId = forumId
          , ListForumPosts.configSession = session
          , ListForumPosts.configPageSize = mPageSize
          , ListForumPosts.configPageNumber = mPage
          })

    createForum :: SessionData -> CreateForumRequest -> AppHandler (WithCSRFToken ForumResponse)
    createForum session request = 
      CreateForum.execute session request >>= addCsrfToken
