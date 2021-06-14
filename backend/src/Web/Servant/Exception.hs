{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs       #-}
module Web.Servant.Exception
    ( SomeServantException(..)
    , AsServantException(..)
    , ServantException(..)
    )
    where

import           Control.Exception
import           Data.Coerce       (coerce)
import           Data.Typeable     (cast)
import           Servant

data SomeServantException where
    SomeServantException :: AsServantException e => e -> SomeServantException

instance Show SomeServantException where
    show (SomeServantException e) = show e
instance Exception SomeServantException

class Exception e => AsServantException e where
    asServantException :: e -> ServerError

newtype ServantException e = ServantException e
    deriving (Show, AsServantException) via e

instance (Exception e, AsServantException e) => Exception (ServantException e) where
    toException = toException . SomeServantException
    fromException x = do
        SomeServantException a <- fromException x
        cast a
