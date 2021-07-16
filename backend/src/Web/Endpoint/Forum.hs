{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  where

import           Data.Int

import           Web.AppHandler
import           Web.Auth
import           Web.Errors
import           Web.Obfuscate
import           Web.Servant.Csrf
import           Web.Servant.Obfuscate

import           Control.Monad.Reader            (MonadReader)
import           Web.Eved
import           Web.Eved.Auth
import qualified Web.Eved.ContentType            as CT
import qualified Web.Eved.QueryParam             as QP
import qualified Web.Eved.UrlElement             as UE

import           Data.PaginatedResponse

import           DB.Model.Forum
import           Domain.Types.CreateForumRequest as CreateForumRequest
import           Domain.Types.ForumPostResponse  as ForumPostResponse
import           Domain.Types.ForumResponse      as ForumResponse
import           Domain.Types.SessionData        as SessionData

import qualified UseCase.Forum.CreateForum       as CreateForum
import qualified UseCase.Forum.GetForum          as GetForum
import qualified UseCase.Forum.ListForumPosts    as ListForumPosts
import qualified UseCase.Forum.ListForums        as ListForums


type Api m =
       (SessionData -> Int64 -> Int64 -> m (PaginatedResponse ForumResponse))
  :<|> (SessionData -> ForumId -> m ForumResponse)
  :<|> (SessionData -> ForumId -> Int64 -> Int64 -> m (PaginatedResponse ForumPostResponse))
  :<|> (SessionData -> CreateForumRequest -> m ForumResponse)

api :: ( MonadReader ctx r
       , Eved api m
       , EvedAuth api
       , HasAuthScheme ctx SessionData
       ) => r (api (Api m))
api = lit "forums" .</> (listForumsApi .<|> getForumApi .<|> getForumPostsApi .<|> createForumApi)
  where
    pagination next =
            queryParam "pageSize" (QP.defaulted 25 (QP.auto @_ @Int64))
       .</> queryParam "page" (QP.defaulted 1 (QP.auto @_ @Int64))
       .</> next

    listForumsApi =
        protected
        .</> pagination
        .</> get [CT.json @(PaginatedResponse ForumResponse)]

    getForumApi =
        protected
        .</> capture "forumId" (UE.auto @ForumId)
        .</> get [CT.json @ForumResponse]

    getForumPostsApi =
        protected
        .</> capture "forumId" (UE.auto @ForumId)
        .</> pagination
        .</> get [CT.json @(PaginatedResponse ForumPostResponse)]

    createForumApi =
        protected
        .</> reqBody [CT.json @CreateForumRequest]
        .</> post [CT.json @ForumResponse]


server :: Api AppHandler
server = listForums :<|> getForum :<|> getForumPosts :<|> createForum
  where
    listForums :: SessionData
               -> Int64
               -> Int64
               -> AppHandler (PaginatedResponse ForumResponse)
    listForums session pageSize page = do
      -- addCsrfToken =<<
        ListForums.execute (ListForums.Config
          { ListForums.configSession = session
          , ListForums.configPageSize = Just pageSize
          , ListForums.configPageNumber = Just page
          })

    getForum :: SessionData -> ForumId -> AppHandler ForumResponse
    getForum sessionData forumId = do
      GetForum.execute sessionData forumId
        >>= maybeNotFound
        -- >>= addCsrfToken

    getForumPosts :: SessionData
                  -> ForumId
                  -> Int64
                  -> Int64
                  -> AppHandler (PaginatedResponse ForumPostResponse)
    getForumPosts session forumId pageSize page = do
      --addCsrfToken =<<
        ListForumPosts.execute (ListForumPosts.Config
          { ListForumPosts.configForumId = forumId
          , ListForumPosts.configSession = session
          , ListForumPosts.configPageSize = Just pageSize
          , ListForumPosts.configPageNumber = Just page
          })

    createForum :: SessionData -> CreateForumRequest -> AppHandler ForumResponse
    createForum session request =
      CreateForum.execute session request -- >>= addCsrfToken
