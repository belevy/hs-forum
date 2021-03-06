module Database.Esqueleto.Extended ( module E
                                   , selectFirst
                                   , selectCount
                                   , update
                                   , updateCount
                                   )
  where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor                         (($>))
import           Data.Int                             (Int64)
import qualified Data.Maybe                           as Maybe
import           Database.Esqueleto.Experimental      as E hiding (selectFirst,
                                                            update, updateCount)
import           Database.Esqueleto.Internal.Internal (Mode (..), SqlSelect,
                                                       rawEsqueleto)

selectFirst :: (MonadIO m, SqlSelect a r) => SqlQuery a -> SqlReadT m (Maybe r)
selectFirst q =
  Maybe.listToMaybe <$> select (q <* limit 1)

update :: MonadIO m => SqlQuery () -> SqlWriteT m ()
update = void . updateCount

updateCount :: MonadIO m => SqlQuery () -> SqlWriteT m Int64
updateCount = rawEsqueleto UPDATE

selectCount :: (MonadIO m) => SqlQuery a -> SqlReadT m Int64
selectCount q =
  unValue . head <$> select (q $> countRows)
