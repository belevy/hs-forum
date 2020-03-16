{-# LANGUAGE TemplateHaskell #-}

module Web.Endpoint.Forum 
  ( Api
  , api
  , server
  ) where

import Servant
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import UnliftIO (liftIO, throwIO)
import qualified Data.Maybe as Maybe

import Web.Obfuscate
import Web.AppHandler
import Web.Errors
import Env

import Database.Persist (Entity(..))
import DB.Forum
import DB.Model.Forum
import DB.Model.ForumPost
import DB.Model.User

type Api = "forums" :> ObfuscatedCapture "forumId" ForumId :> ObfuscatedGet '[JSON] ForumResponse
      :<|> "forums" :> ObfuscatedCapture "forumId" ForumId 
                    :> "posts" 
                    :> QueryParam "page" Int
                    :> QueryParam "pageSize" Int
                    :> ObfuscatedGet '[JSON] (PaginatedResponse ForumPostResponse)

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

data PaginatedResponse a = PaginatedResponse
  { paginatedTotalCount :: Int
  , paginatedCurrentPage :: Int
  , paginatedPageSize :: Int
  , paginatedData :: [a]
  }

api :: Proxy Api
api = Proxy 

server :: AppServer Api
server = getForum  :<|> getForumPosts
  where
    getForum :: ForumId -> AppHandler ForumResponse
    getForum forumId = do
      liftIO $ putStrLn "hi"
      mForum <- runDB $ getForumById forumId
      (Entity forumId forum, creator, admins) <- maybeNotFound mForum
      pure $ ForumResponse
        { frForumId = forumId
        , frName = forumName forum
        , frDescription = forumDescription forum
        , frCreator = userToForumAdministrator creator
        , frAdministrators = fmap userToForumAdministrator admins
        }

    getForumPosts :: ForumId 
                  -> Maybe Int 
                  -> Maybe Int 
                  -> AppHandler (PaginatedResponse ForumPostResponse)
    getForumPosts forumId mPage mPageSize = do
      (totalPosts, currentPosts) <- runDB $ getTopPosts forumId page pageSize
      pure $ PaginatedResponse
        { paginatedTotalCount = fromIntegral totalPosts
        , paginatedCurrentPage = page
        , paginatedPageSize = pageSize
        , paginatedData = [] 
        }

      where
        page = Maybe.fromMaybe 1 mPage
        pageSize = Maybe.fromMaybe 25 mPageSize

userToForumAdministrator :: Entity User -> ForumAdministrator
userToForumAdministrator (Entity userId user) =
  ForumAdministrator 
    { faUserId = userId
    , faUserName = userUserName user
    }


$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fa")} 'ForumAdministrator)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fr")} 'ForumResponse)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "paginated")} 'PaginatedResponse)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "fpr")} 'ForumPostResponse)
