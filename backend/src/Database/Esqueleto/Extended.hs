module Database.Esqueleto.Extended ( module E
                                   , selectFirst
                                   , selectCount
                                   , update
                                   , updateCount
                                   )
  where 

import Control.Monad
import Control.Monad.IO.Class
import Database.Esqueleto as E hiding (update, updateCount, from, on, selectFirst)
import Database.Esqueleto.Experimental as E hiding (From)
import Database.Esqueleto.Internal.Sql (SqlSelect)
import Database.Esqueleto.Internal.Internal (rawEsqueleto, Mode(..))
import qualified Data.Maybe as Maybe
import Data.Int (Int64)

selectFirst :: (MonadIO m, SqlSelect a r) => SqlQuery a -> SqlReadT m (Maybe r)
selectFirst q =
  fmap Maybe.listToMaybe $ 
    select (q <* limit 1)

update :: MonadIO m => SqlQuery () -> SqlWriteT m ()
update = void . updateCount

updateCount :: MonadIO m => SqlQuery () -> SqlWriteT m Int64
updateCount = rawEsqueleto UPDATE

selectCount :: (MonadIO m) => SqlQuery a -> SqlReadT m Int64
selectCount q =
  fmap (unValue . head) $
  select (q *> pure countRows)
