{-# LANGUAGE OverloadedStrings #-}

module Yael.Eff.Postgres.Traced where

import           Control.Lens
import qualified Data.Text.Encoding               as T
import           Database.PostgreSQL.Simple.Types
import           OpenTracing
import           OpenTracing.Tags
import           Yael.Eff
import           Yael.Eff.OpenTracing
import           Yael.Eff.Postgres

traceSql :: forall m . Monad m => Tracing m -> Sql m -> Sql m
traceSql t s = Sql
  { _query = \q r -> run $ do
      opts <- mkSpanOpts "query" $ \opts -> opts
        & spanOptTags .~ [toStatementTag q]
      spanning opts $ query q r

  , _query_ = \q -> run $ do
      opts <- mkSpanOpts "query_" $ \opts -> opts
        & spanOptTags .~ [toStatementTag q]
      spanning opts $ query_ q

  , _execute = \q r -> run $ do
      opts <- mkSpanOpts "execute" $ \opts -> opts
        & spanOptTags .~ [toStatementTag q]
      spanning opts $ execute q r

  , _execute_ = \q -> run $ do
      opts <- mkSpanOpts "execute_" $ \opts -> opts
        & spanOptTags .~ [toStatementTag q]
      spanning opts $ execute_ q
  }

  where
    run :: EffT (Tracing :<> Sql) m a -> m a
    run = flip runEffT (t :<> s)

    toStatementTag :: Query -> Tag
    toStatementTag = DbStatement . T.decodeUtf8 . fromQuery
