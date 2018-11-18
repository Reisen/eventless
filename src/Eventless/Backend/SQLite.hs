module Eventless.Backend.SQLite
  ( makeSQLite3Backend
  )
where

import           Protolude
import           Data.Aeson                     ( FromJSON, decode )
import           Data.UUID                      ( UUID, toText )
import           Database.SQLite.Simple         ( Connection
                                                , Only(..)
                                                , Query
                                                , query
                                                , execute
                                                , execute_
                                                , withTransaction
                                                )

import           Eventless                      ( Aggregate(..)
                                                , BackendStore(..)
                                                , Event(..)
                                                )


makeSQLite3Backend
  :: Connection
  -> BackendStore

makeSQLite3Backend conn = Backend
  { loadLatest            = sqliteLoadLatest conn
  , loadVersion           = sqliteLoadVersion conn
  , writeEventTransaction = sqliteWriteEventTransaction conn
  }


createEventsTable
  :: MonadIO m
  => Connection
  -> m ()

createEventsTable =
  liftIO . flip execute_ sql_CreateEventsIfNoExist


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


sql_FetchLatestAggregateUUID :: Query
sql_FetchLatestAggregateUUID = "\
\ SELECT version, snapshot FROM events \
\ WHERE uuid = ?                       \
\ ORDER BY version DESC                \
\ LIMIT 1                              \
\ "


sql_FetchAggregateByVersion :: Query
sql_FetchAggregateByVersion = "\
\ SELECT version, snapshot FROM events \
\ WHERE                                \
\   uuid    = ? AND                    \
\   version = ?                        \
\ LIMIT 1                              \
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
