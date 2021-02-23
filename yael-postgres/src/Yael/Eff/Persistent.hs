module Yael.Eff.Persistent where

import Yael.Eff
import Yael.Eff.Reader
import Yael.Eff.Postgres
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader (mapReaderT, ReaderT(..))
import Database.Persist (PersistValue)
import Database.Persist.Sql (SqlPersistT, runSqlPool, runSqlConn, SqlBackend)
import qualified Database.Persist.Sql as Persist
import Database.Persist.Postgresql (openSimpleConn)
import Data.Pool
import UnliftIO
import Control.Monad.Logger
import Data.Text (Text)

data Persistent m = Persistent
  { _runPersistent :: forall a . SqlPersistT m a -> m a
  , _rawExecute
      :: Text            -- ^ SQL statement, possibly with placeholders.
      -> [PersistValue]  -- ^ Values to fill the placeholders.
      -> SqlPersistT m ()
  }

runPersistent :: (HasEff Persistent f m) => SqlPersistT (EffT f m) a -> EffT f m a
runPersistent p = withEffT' $ \lower Persistent{_runPersistent} -> _runPersistent $ mapReaderT lower p

rawExecute :: (HasEff Persistent f m) => Text -> [PersistValue] -> SqlPersistT (EffT f m) ()
rawExecute query values = ReaderT $ \backend ->
  withEffT $ \Persistent{_rawExecute} -> flip runReaderT backend $ _rawExecute query values

poolPersistent :: (MonadUnliftIO m) => Pool SqlBackend -> Persistent m
poolPersistent pool = Persistent
  { _runPersistent = flip runSqlPool pool
  , _rawExecute = Persist.rawExecute
  }

-- defer any persistent actions to a larger context. This is useful in situations
-- where your code might call `runPersistent` in a nested way to avoid breaking your transaction
readerTPersistent :: MonadIO m => Persistent (SqlPersistT m)
readerTPersistent = Persistent
  { _runPersistent = joinReaderT
  , _rawExecute = Persist.rawExecute
  }

readerPersistent :: MonadIO m => Reader SqlBackend m -> Persistent m
readerPersistent rdr = Persistent
  { _runPersistent = \sqlT -> _ask rdr >>= runReaderT sqlT
  , _rawExecute = Persist.rawExecute
  }

joinReaderT :: ReaderT r (ReaderT r m) a -> ReaderT r m a
joinReaderT action = ReaderT $ \r -> runReaderT (runReaderT action r) r

connPersistent :: MonadUnliftIO m => SqlBackend -> Persistent m
connPersistent conn = Persistent
  { _runPersistent = flip runSqlConn conn
  , _rawExecute = Persist.rawExecute
  }

postgresPersistent :: (MonadUnliftIO m, MonadLoggerIO m) => Postgres m -> Persistent m
postgresPersistent Postgres{_withSqlConn} = Persistent
  { _runPersistent = \p -> _withSqlConn $ \conn -> do
      logFunc <- askLoggerIO
      pConn <- liftIO $ openSimpleConn logFunc conn
      runSqlConn p pConn
  , _rawExecute = Persist.rawExecute
  }
