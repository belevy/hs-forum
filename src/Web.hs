{-# LANGUAGE TemplateHaskell #-}

module Web where

import Control.Monad (Monad)
import Control.Monad.IO.Class (liftIO)
import Servant
import Servant.Server
import qualified Network.Wai.Handler.Warp as Warp
import Data.UserCredentials (UserCredentials)
import qualified Data.UserCredentials as UserCredentials
import Data.UserCredentials
import DB.User
import DB.Model.User (UserId)
import qualified Config
import Env
import Web.AppHandler
import Database.Persist.Postgresql
import Web.Obfuscate
import qualified Data.Text as T
import Data.Aeson (defaultOptions, camelTo2, fieldLabelModifier)
import Data.Aeson.TH
import Hashids


type Api = "session" :> ObfuscatedCapture "userId" UserId :> Post '[JSON] ()
        :<|>  "test" :> ObfuscatedVerb GET 200 '[JSON] TestJSON

server :: AppServer Api 
server = session :<|> test
  where
  session userId = do
    liftIO $ print userId
    runDB $ do
      pure ()

  test = 
    pure $ TestJSON
      { _testId = toSqlKey 1
      , _testManyIds = fmap toSqlKey [1,2,3,4,5]
      }

api :: Proxy Api
api = Proxy

mkApp :: Config.Config -> IO Application
mkApp config = 
  withEnv config $ \env -> 
  let Right hashidsCtx = mkHashidsContext "" 6 defaultAlphabet
      context = hashidsCtx :. EmptyContext
      contextProxy = Proxy @'[HashidsContext]
  in do 
    liftIO $ print $ encode hashidsCtx [1]
    pure $ serveWithContext api context
          $ hoistServerWithContext api contextProxy (runAppHandler env) server
           
runApp :: IO ()
runApp = do
    config <- Config.loadConfigFile
    app <- mkApp config
    putStrLn $ "Running on " <> (show $ Config.configPort config)
    Warp.run (Config.configPort config) app 

data TestJSON = TestJSON
  { _testId :: UserId
  , _testManyIds :: [UserId]
  }

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop (T.length "_test")} 'TestJSON)
