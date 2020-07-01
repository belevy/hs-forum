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
import Database.Esqueleto as E (Entity(..), Value(..))

import Web.Obfuscate
import Web.AppHandler
import Web.Errors
import Web.Auth

import Data.PaginatedResponse
import Env
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

data ObfuscatedForumResponse = ObfuscatedForumResponse 
  { ofrForumId :: Obfuscated ForumId
  , ofrName :: Text
  , ofrDescription :: Text
  , ofrCreator :: Obfuscated ForumAdministrator
  , ofrAdministrators :: Obfuscated [ForumAdministrator]
  }

instance Obfuscateable ForumResponse where
  type Obfuscated ForumResponse = ObfuscatedForumResponse
  obfuscate ctx response = ObfuscatedForumResponse
    { ofrForumId = obfuscate ctx $ frForumId response
    , ofrName = frName response 
    , ofrDescription = frDescription response
    , ofrCreator = obfuscate ctx $ frCreator response
    , ofrAdministrators = obfuscate ctx $ frAdministrators response 
    }
  deobfuscate ctx response = do
    forumId <- deobfuscate ctx $ ofrForumId response
    creator <- deobfuscate ctx $ ofrCreator response
    admins <- deobfuscate ctx $ ofrAdministrators response 
    pure $ ForumResponse
      { frForumId = forumId
      , frName = ofrName response 
      , frDescription = ofrDescription response
      , frCreator = creator
      , frAdministrators = admins
      }
    
data ForumAdministrator = ForumAdministrator
  { faUserId :: UserId
  , faUserName :: Text
  }
data ObfuscatedForumAdministrator = ObfuscatedForumAdministrator
  { ofaUserId :: Obfuscated UserId
  , ofaUserName :: Text
  }

instance Obfuscateable ForumAdministrator where
  type Obfuscated ForumAdministrator = ObfuscatedForumAdministrator
  obfuscate ctx response = ObfuscatedForumAdministrator
    { ofaUserId = obfuscate ctx $ faUserId response
    , ofaUserName = faUserName response
    }
  deobfuscate ctx response = do 
    userId <- deobfuscate ctx $ ofaUserId response
    pure $ ForumAdministrator
      { faUserId = userId
      , faUserName = ofaUserName response
      }

data ForumPostResponse = ForumPostResponse
  { fprForumPostId :: ForumPostId
  , fprContent :: Text
  , fprSortOrder :: Int
  , fprCreated :: UTCTime
  }

data ObfuscatedForumPostResponse = ObfuscatedForumPostResponse
  { ofprForumPostId :: Obfuscated ForumPostId
  , ofprContent :: Text
  , ofprSortOrder :: Int
  , ofprCreated :: UTCTime
  }
instance Obfuscateable ForumPostResponse where
  type Obfuscated ForumPostResponse = ObfuscatedForumPostResponse
  obfuscate ctx response = ObfuscatedForumPostResponse
      { ofprForumPostId = obfuscate ctx $ fprForumPostId response
      , ofprContent = fprContent response 
      , ofprSortOrder = fprSortOrder response 
      , ofprCreated = fprCreated response 
      }
  deobfuscate ctx response = do 
    forumPostId <- deobfuscate ctx $ ofprForumPostId response
    pure $ ForumPostResponse
      { fprForumPostId= forumPostId 
      , fprContent = ofprContent response 
      , fprSortOrder = ofprSortOrder response 
      , fprCreated = ofprCreated response 
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

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofa")} 'ObfuscatedForumAdministrator)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofr")} 'ObfuscatedForumResponse)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "ofpr")} 'ObfuscatedForumPostResponse)
