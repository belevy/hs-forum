{-# LANGUAGE LambdaCase #-}
module Web.Eved.Obfuscate
    where

import           Control.Monad.Reader

import qualified Web.Eved.ContentType as CT
import qualified Web.Eved.Header      as H
import qualified Web.Eved.QueryParam  as QP
import qualified Web.Eved.UrlElement  as UE

import           Data.Aeson           (FromJSON, ToJSON, Value (..),
                                       eitherDecode, encode)
import           Data.Bifunctor       (first)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Web.HttpApiData
import           Web.Obfuscate

import qualified Hashids

class HasHashidsContext env where
    getHashidsContext :: env -> Hashids.HashidsContext

obfuscateQP ::
    ( MonadReader ctx r
    , HasHashidsContext ctx
    , Obfuscated a ~ Value
    , CanObfuscate a
    , CanDeobfuscate a
    ) => r (QP.QueryParam a)
obfuscateQP = do
    qp <- QP.auto
    ctx <- asks getHashidsContext
    pure $ QP.QueryParam
        { QP.fromQueryParam = \p -> do
            val <- QP.fromQueryParam qp p
            case deobfuscate ctx (String val) of
              Just a  -> Right a
              Nothing -> Left "Failed To deobfuscate"
        , QP.toQueryParam = \a ->
            case obfuscate ctx a of
              String txt -> QP.toQueryParam qp txt
              _          -> mempty
        }

obfuscateUE :: forall a ctx r.
    ( MonadReader ctx r
    , HasHashidsContext ctx
    , Obfuscated a ~ Value
    , CanObfuscate a
    , CanDeobfuscate a
    ) => r (UE.UrlElement a)
obfuscateUE = do
    ue <- UE.auto @Text
    ctx <- asks getHashidsContext
    pure $ UE.UrlElement
        { UE.fromUrlPiece = \p -> do
            val <- UE.fromUrlPiece ue p
            case deobfuscate ctx (String val) of
              Just a  -> Right a
              Nothing -> Left "Failed To deobfuscate"
        , UE.toUrlPiece = \a ->
            case obfuscate ctx a of
              String txt -> UE.toUrlPiece ue txt
              _          -> mempty
        }

obfuscatedJSON :: forall a ctx r.
    ( MonadReader ctx r
    , HasHashidsContext ctx
    , CanObfuscate a
    , CanDeobfuscate a
    , ToJSON (Obfuscated a)
    , FromJSON (Obfuscated a)
    ) => r (CT.ContentType a)
obfuscatedJSON = obfuscateCT CT.json

obfuscateCT ::
    ( MonadReader ctx r
    , HasHashidsContext ctx
    , CanObfuscate a
    , CanDeobfuscate a
    ) => r (CT.ContentType (Obfuscated a))
      -> r (CT.ContentType a)
obfuscateCT fct = do
    ct <- fct
    ctx <- asks getHashidsContext
    pure $ ct
        { CT.toContentType   = CT.toContentType ct . obfuscate ctx
        , CT.fromContentType = \r -> do
            val <- CT.fromContentType ct r
            case deobfuscate ctx val of
              Just a  -> Right a
              Nothing -> Left "Failed To deobfuscate"
        }
