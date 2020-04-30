module Web.Endpoint.Session 
  ( Api
  , server
  ) where

import Servant
import Web.Auth
import Web.Obfuscate
import Web.AppHandler
import Data.SessionData

type Api = "sessions" :> 
    Protected :> ObfuscatedGet '[JSON] SessionData

server :: AppServer Api
server = currentSession
  where
    currentSession session = pure session
