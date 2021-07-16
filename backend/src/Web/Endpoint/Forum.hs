{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum
  where

import           Data.Int

import           Web.AppHandler
import           Web.Auth
import           Web.Errors
import           Web.HttpApiData
import           Web.Obfuscate
import           Web.Servant.Csrf
import           Web.Servant.Obfuscate

import           Control.Monad.Reader            (MonadReader)
import           Web.Eved
import           Web.Eved.Auth
import qualified Web.Eved.ContentType            as CT
import           Web.Eved.Obfuscate
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



newtype PageSize = PageSize Int64
    deriving newtype (Eq, Ord, Num, ToHttpApiData, FromHttpApiData)
newtype PageNumber = PageNumber Int64
    deriving newtype (Eq, Ord, Num, ToHttpApiData, FromHttpApiData)

type Api m =
         ListForums m
    :<|> GetForum m
    :<|> ListForumPosts m
    :<|> CreateForum m

type ListForums m =
    SessionData -> PageSize -> PageNumber -> m (PaginatedResponse ForumResponse)
type GetForum m =
    SessionData -> ForumId -> m ForumResponse
type ListForumPosts m =
    SessionData -> ForumId -> PageSize -> PageNumber -> m (PaginatedResponse ForumPostResponse)
type CreateForum m =
    SessionData -> CreateForumRequest -> m ForumResponse

api :: ( MonadReader ctx r
       , Eved api m
       , EvedAuth api
       , HasAuthScheme ctx SessionData
       , HasHashidsContext ctx
       ) => r (api (Api m))
api =
    lit "forums" .</>
          (    listForumsApi
          .<|> getForumApi
          .<|> getForumPostsApi
          .<|> createForumApi
          )
  where
    forumIdUE =
        obfuscateUE @ForumId

    pagination next = do
            queryParam "pageSize" (QP.defaulted 25 (QP.auto @_ @PageSize))
       .</> queryParam "page" (QP.defaulted 1 (QP.auto @_ @PageNumber))
       .</> next

    listForumsApi =
        protected
        .</> pagination
        .</> get [obfuscatedJSON @(PaginatedResponse ForumResponse)]

    getForumApi =
        protected
        .</> capture "forumId" forumIdUE
        .</> get [obfuscatedJSON @ForumResponse]

    getForumPostsApi =
        protected
        .</> capture "forumId" forumIdUE
        .</> pagination
        .</> get [obfuscatedJSON @(PaginatedResponse ForumPostResponse)]

    createForumApi =
        protected
        .</> reqBody [obfuscatedJSON @CreateForumRequest]
        .</> post [obfuscatedJSON @ForumResponse]


server :: Api AppHandler
server = listForums :<|> getForum :<|> getForumPosts :<|> createForum
  where
    listForums :: ListForums AppHandler
    listForums session (PageSize pageSize) (PageNumber page) = do
      -- addCsrfToken =<<
        ListForums.execute (ListForums.Config
          { ListForums.configSession = session
          , ListForums.configPageSize = Just pageSize
          , ListForums.configPageNumber = Just page
          })

    getForum :: GetForum AppHandler
    getForum sessionData forumId = do
      GetForum.execute sessionData forumId
        >>= maybeNotFound
        -- >>= addCsrfToken

    getForumPosts :: ListForumPosts AppHandler
    getForumPosts session forumId (PageSize pageSize) (PageNumber page) = do
      --addCsrfToken =<<
        ListForumPosts.execute (ListForumPosts.Config
          { ListForumPosts.configForumId = forumId
          , ListForumPosts.configSession = session
          , ListForumPosts.configPageSize = Just pageSize
          , ListForumPosts.configPageNumber = Just page
          })

    createForum :: CreateForum AppHandler
    createForum session request =
      CreateForum.execute session request -- >>= addCsrfToken
