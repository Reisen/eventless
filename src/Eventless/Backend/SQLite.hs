module Eventless.Backend.SQLite
  ( makeSQLite3Backend
  ) where

import Protolude
import Data.Aeson             ( FromJSON, decode )
import Data.UUID              ( UUID, toText, fromText )
import Database.SQLite.Simple ( Connection
                              , Only (..)
                              , fromOnly
                              , Query
                              , query
                              , query_
                              , execute
                              , execute_
                              , withTransaction
                              )

import Eventless              ( Aggregate (..)
                              , BackendStore (..)
                              , Event (..)
                              )

--------------------------------------------------------------------------------

makeSQLite3Backend
  :: Connection
  -> BackendStore

makeSQLite3Backend conn = Backend
  { loadAggregates        = sqliteLoadAggregates conn
  , loadEvents            = sqliteLoadEvents conn
  , loadLatest            = sqliteLoadLatest conn
  , loadVersion           = sqliteLoadVersion conn
  , writeEventTransaction = sqliteWriteEventTransaction conn
  }

--------------------------------------------------------------------------------

-- | Helper for creating the events table if it doesn't already exist. Runs
-- | on every transaction.
createEventsTable
  :: MonadIO m
  => Connection
  -> m ()

createEventsTable =
  liftIO . flip execute_ sql_CreateEventsIfNoExist

--------------------------------------------------------------------------------

sqliteLoadLatest
  :: FromJSON a
  => MonadIO m
  => Connection
  -> UUID
  -> m (Maybe (Aggregate a))

sqliteLoadLatest conn uuid = do
  createEventsTable conn
  result <- liftIO $ query conn sql_FetchLatestAggregateUUID (Only $ toText uuid)
  pure $ do
    (version, agg) <- head result
    decoded        <- decode (toS (agg :: Text))
    pure Aggregate
      { aggregateUUID    = uuid
      , aggregateVersion = version
      , aggregateValue   = decoded
      }


sqliteLoadVersion
  :: FromJSON a
  => MonadIO m
  => Connection
  -> UUID
  -> Int
  -> m (Maybe (Aggregate a))

sqliteLoadVersion conn uuid targetVersion = do
  createEventsTable conn
  result <- liftIO $ query conn sql_FetchAggregateByVersion (toText uuid, targetVersion)
  pure $ do
    (version, agg) <- head result
    decoded        <- decode (toS (agg :: Text))
    pure Aggregate
      { aggregateUUID    = uuid
      , aggregateVersion = version
      , aggregateValue   = decoded
      }


sqliteLoadEvents
  :: MonadIO m
  => Connection
  -> UUID
  -> m [Event]

sqliteLoadEvents conn uuid = do
  createEventsTable conn
  result <- liftIO $ query conn sql_FetchEventsByUUID (Only $ toText uuid)
  pure $ result <&> \(kind, emitted, version, event, event_body, snapshot) -> do
    Event
      { eventKind     = kind
      , eventEmitted  = emitted
      , eventVersion  = version
      , eventName     = event
      , eventBody     = event_body
      , eventSnapshot = snapshot
      }


sqliteLoadAggregates
  :: MonadIO m
  => Connection
  -> m [UUID]

sqliteLoadAggregates conn = do
  createEventsTable conn
  result :: [Only Text] <- liftIO $ query_ conn sql_FetchAggregateUUIDs
  pure $ catMaybes $ fromText . fromOnly <$> result


sqliteWriteEventTransaction
  :: Traversable t
  => MonadIO m
  => Connection
  -> UUID
  -> t Event
  -> m ()

sqliteWriteEventTransaction conn uuid events = do
  createEventsTable conn
  liftIO $ withTransaction conn $ for_ events $ \Event {..} ->
    execute conn sql_WriteEventForUUID
      ( toText uuid
      , eventKind
      , eventEmitted
      , eventVersion
      , eventName
      , eventBody
      , eventSnapshot
      )

-- SQL Statements --------------------------------------------------------------

sql_CreateEventsIfNoExist :: Query
sql_CreateEventsIfNoExist = "       \
\ CREATE TABLE IF NOT EXISTS events \
\   ( uuid       TEXT     NOT NULL  \
\   , kind       TEXT     NOT NULL  \
\   , emitted    TEXT     NOT NULL  \
\   , version    INTEGER  NOT NULL  \
\   , event      TEXT     NOT NULL  \
\   , event_body TEXT     NOT NULL  \
\   , snapshot   TEXT     NOT NULL  \
\   )"


sql_FetchLatestAggregateUUID :: Query
sql_FetchLatestAggregateUUID = "\
\ SELECT    version, snapshot   \
\ FROM      events              \
\ WHERE     uuid = ?            \
\ ORDER BY  version DESC        \
\ LIMIT     1                   \
\ "


sql_FetchAggregateByVersion :: Query
sql_FetchAggregateByVersion = "\
\ SELECT version, snapshot     \
\ FROM   events                \
\ WHERE  uuid    = ?           \
\ AND    version = ?           \
\ LIMIT  1                     \
\ "


sql_FetchAggregateUUIDs :: Query
sql_FetchAggregateUUIDs = "\
\ SELECT DISTINCT uuid     \
\ FROM   events            \
\ "


sql_WriteEventForUUID :: Query
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


sql_FetchEventsByUUID :: Query
sql_FetchEventsByUUID = "\
\ SELECT kind            \
\      , emitted         \
\      , version         \
\      , event           \
\      , event_body      \
\      , snapshot        \
\ FROM   events          \
\ WHERE  uuid = ?        \
\ "
