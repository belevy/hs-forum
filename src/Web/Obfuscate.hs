{-# LANGUAGE BangPatterns #-}

module Web.Obfuscate where

import System.IO.Unsafe

import Data.Kind
import Text.Read (readMaybe)
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat, natVal, KnownNat, KnownSymbol, Symbol)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Maybe as Maybe
import Data.Int (Int64)

import qualified Hashids as H
import Web.HttpApiData
import Servant
import Servant.Server.Internal
import Servant.API.ContentTypes
import Network.HTTP.Types (hContentType, hAccept, Method, Status, HeaderName)
import Network.Wai (responseLBS, requestHeaders)
import Database.Persist
import Database.Persist.Sql
import Data.Aeson (Value(..), ToJSON(..))
import qualified Data.ByteString as B
import Data.Scientific (isInteger, toBoundedInteger)
import Data.HashMap.Strict (mapWithKey)

data ObfuscatedCapture (sym :: Symbol) (a :: Type) 
  deriving (Typeable)

instance {-# OVERLAPPABLE #-} 
      (HasContextEntry context H.HashidsContext, KnownSymbol capture, HasServer api context, Integral a)
      => HasServer (ObfuscatedCapture capture a :> api) context where

  type ServerT (ObfuscatedCapture capture a :> api) m = a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case (H.decode hashidsContext (T.unpack txt), readMaybe (T.unpack txt)) of
                  (v':_, _) -> return (fromIntegral v')
                  ([], Just v') -> return (fromIntegral v')
                  ([], Nothing) -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
            )

instance (HasContextEntry context H.HashidsContext, KnownSymbol capture, HasServer api context, ToBackendKey SqlBackend e)
      => HasServer (ObfuscatedCapture capture (Key e) :> api) context where

  type ServerT (ObfuscatedCapture capture (Key e) :> api) m = (Key e) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    let hashidsContext = getContextEntry context
    in CaptureRouter $
      route (Proxy :: Proxy api)
            context
            (addCapture d $ \txt -> 
               case (H.decode hashidsContext (T.unpack txt), readMaybe (T.unpack txt)) of
                  (v':_, _) -> return (toSqlKey $ fromIntegral v')
                  ([], Nothing) -> delayedFail err400 { errBody = "Failed to deobfuscate entry" }
                  ([], Just v') -> return (toSqlKey $ fromIntegral v')
            )

data ObfuscatedVerb (method :: StdMethod) (status :: Nat) (ctypes :: [Type]) (a :: Type)
  deriving (Typeable)

instance {-# OVERLAPPABLE #-}
         ( ToJSON a, HasContextEntry context H.HashidsContext, ReflectMethod method, KnownNat status
         ) => HasServer (ObfuscatedVerb method status '[JSON] a) context where

  type ServerT (ObfuscatedVerb method status '[JSON] a) m = m a
  hoistServerWithContext _ _ nt s = nt s

  route Proxy context = obfuscatedMethodRouter (getContextEntry context) ((,)[]) method (Proxy :: Proxy '[JSON]) status
      where method = reflectMethod (Proxy :: Proxy method)
            status = toEnum . fromInteger $ natVal (Proxy :: Proxy status)

obfuscatedMethodRouter :: (ToJSON a) 
                       => H.HashidsContext
                       -> (b -> ([(HeaderName, B.ByteString)], a))
                       -> Method -> Proxy '[JSON] -> Status
                       -> Delayed env (Handler b)
                       -> Router env
obfuscatedMethodRouter hashidsContext splitHeaders method proxy status action = leafRouter route'
  where
    route' env request respond =
          let accH = Maybe.fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          in runAction (action `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                       ) env request respond $ \ output -> do
               let (headers, b) = splitHeaders output
               case handleAcceptH proxy (AcceptHeader accH) (encodeIdFields hashidsContext b) of
                 Nothing -> FailFatal err406 -- this should not happen (checked before), so we make it fatal if it does
                 Just (contentT, body) ->
                      let bdy = if allowedMethodHead method request then "" else body
                      in Route $ responseLBS status ((hContentType, cs contentT) : headers) bdy

encodeIdFields :: ToJSON a => H.HashidsContext -> a -> Value
encodeIdFields ctx = go 0 . toJSON
  where
  go i (Object oMap) = Object $ flip mapWithKey oMap $ \fieldName fieldValue -> 
      case fieldValue of
        v@(Object _) -> 
          go (i+1) v
        v@(Array a) -> 
          if "ids" `T.isSuffixOf` fieldName then 
            go (i+1) v
          else 
            v
        v@(Number n) -> 
          if "id" `T.isSuffixOf` fieldName then 
            go (i+1) v
          else 
            v
        v -> v

  go i val@(Array a) 
    | i == 0 = val
    | otherwise = Array $ fmap (go i) a

  go i val@(Number n) 
    | i == 0 = val
    | not $ isInteger n = val
    | otherwise = res
    where
      res = 
        Maybe.fromMaybe val $ do 
          i <- toInteger <$> toBoundedInteger @Int64 n
          let encoded = H.encode ctx [i]
          pure $ String $ T.pack encoded

  go _ val = val

