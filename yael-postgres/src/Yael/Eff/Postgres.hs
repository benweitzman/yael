module Yael.Eff.Postgres where

import Yael.Eff
import Database.PostgreSQL.Simple
import qualified Database.PostgreSQL.Simple as P
import Data.Pool
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.Int

newtype Postgres m = Postgres
  { _withSqlConn :: forall a . (Connection -> m a) -> m a
  }

withSqlConn
  :: (HasEff Postgres f m)
  => (Connection -> EffT f m a)
  -> EffT f m a
withSqlConn f = withEffT' $ \lower Postgres{_withSqlConn} ->
  _withSqlConn $ lower . f

connSql :: Connection -> Postgres m
connSql conn = Postgres
  { _withSqlConn = \f -> f conn
  }

poolSql :: (MonadBaseControl IO m) => Pool Connection -> Postgres m
poolSql pool = Postgres
  { _withSqlConn = withResource pool
  }


data Sql (m :: * -> *) = Sql
  { _query
    :: forall q r
     . (ToRow q, FromRow r)
    => Query
    -> q
    -> m [r]

  , _query_
    :: forall r
     . (FromRow r)
    => Query
    -> m [r]

  , _execute :: forall q . (ToRow q) => Query -> q -> m Int64

  , _execute_ :: Query -> m Int64
  }

query :: (ToRow q, FromRow r) => Query -> q -> [r] :+ '[Sql]
query q r = withEffT $ \Sql{_query} -> _query q r

query_ :: (FromRow r) => Query -> [r] :+ '[Sql]
query_ q = withEffT $ \Sql{_query_} -> _query_ q

execute :: (ToRow q) => Query -> q -> Int64 :+ '[Sql]
execute q r = withEffT $ \Sql{_execute} -> _execute q r

execute_ :: Query -> Int64 :+ '[Sql]
execute_ q = withEffT $ \Sql{_execute_} -> _execute_ q

postgresSql :: MonadIO m => Postgres m -> Sql m
postgresSql Postgres{_withSqlConn} = Sql
  { _query = \q r -> _withSqlConn $ \conn -> liftIO $ P.query conn q r
  , _query_ = \q -> _withSqlConn $ \conn -> liftIO $ P.query_ conn q
  , _execute = \q r -> _withSqlConn $ \conn -> liftIO $ P.execute conn q r
  , _execute_ = \q -> _withSqlConn $ \conn -> liftIO $ P.execute_ conn q
  }
