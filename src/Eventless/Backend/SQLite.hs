module Eventless.Backend.SQLite
  ( makeSQLite3Backend
  ) where

import Protolude
import Data.Aeson             (FromJSON, encode, decode)
import Database.SQLite.Simple
  ( Connection
  , Only (..)
  , open
  , query
  , execute
  , execute_
  , withTransaction
  )

import Eventless
  ( Aggregate    (..)
  , BackendStore (..)
  , Event        (..)
  , UUID
  )


makeSQLite3Backend
  :: Connection
  -> BackendStore

makeSQLite3Backend conn =
  Backend
    { loadAggregate  = sqliteLoadAggregate conn
    , writeEvents    = sqliteWriteEvents conn
    }


createEventsTable
  :: MonadIO m
  => Connection
  -> m ()

createEventsTable =
  liftIO . flip execute_ sql_CreateEventsIfNoExist


sqliteLoadAggregate
  :: FromJSON a
  => MonadIO m
  => Connection
  -> UUID
  -> m (Maybe (Aggregate a))

sqliteLoadAggregate conn uuid = do
  createEventsTable conn
  result <- liftIO $ query conn sql_FetchLatestAggregateUUID (Only uuid)
  pure $ do
    (version, agg) <- head result
    decoded        <- decode (toS (agg :: Text))
    pure Aggregate
      { aggregateUUID    = uuid
      , aggregateVersion = version
      , aggregateValue   = decoded
      }


sqliteWriteEvents
  :: Traversable t
  => MonadIO m
  => Connection
  -> UUID
  -> t Event
  -> m ()

sqliteWriteEvents conn uuid events = do
  createEventsTable conn
  liftIO $ withTransaction conn $ for_ events $ \Event{..} ->
    execute conn sql_WriteEventForUUID
      ( uuid
      , eventKind
      , eventEmitted
      , eventVersion
      , eventName
      , eventBody
      , eventSnapshot
      )



-- SQL Statements --------------------------------------------------------------



sql_CreateEventsIfNoExist = "\
\ CREATE TABLE IF NOT EXISTS events ( \
\   uuid       TEXT     NOT NULL,     \
\   kind       TEXT     NOT NULL,     \
\   emitted    TEXT     NOT NULL,     \
\   version    INTEGER  NOT NULL,     \
\   event      TEXT     NOT NULL,     \
\   event_body TEXT     NOT NULL,     \
\   snapshot   TEXT     NOT NULL      \
\ )"


sql_FetchLatestAggregateUUID = "\
\ SELECT version, snapshot FROM events \
\ WHERE uuid = ?                       \
\ ORDER BY version DESC                \
\ LIMIT 1                              \
\ "


sql_WriteEventForUUID = "   \
\ INSERT INTO events        \
\   ( uuid                  \
\   , kind                  \
\   , emitted               \
\   , version               \
\   , event                 \
\   , event_body            \
\   , snapshot              \
\   )                       \
\ VALUES                    \
\   ( ?, ?, ?, ?, ?, ?, ? ) \
\ "
