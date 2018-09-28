module Eventless.Types.Commands
  ( Command
  , CommandContext
  , runCommand
  ) where


import Data.Aeson                (FromJSON, ToJSON)
import Data.Typeable             (Typeable, typeOf)
import Data.Aeson.Text           (encodeToLazyText)
import Control.Monad.Classes
import Control.Monad.Writer.Lazy (WriterT, runWriterT)
import Protolude hiding
  ( MonadReader
  , ask
  )

import Eventless.Types.Aggregate
import Eventless.Types.BackendStore
import Eventless.Types.Event


-- An actual command is just a function that has access to the current
-- aggregate state and produces events based on this. In other words we
-- just run in the Eventless context.
type Command agg m = ReaderT agg (WriterT [Events agg] m) ()

type CommandContext agg m =
  ( MonadReader BackendStore m                 -- ^ We need a context that provides a database backend.
  , MonadExec IO m                             -- ^ We need IO to access that context.
  , MonadIO m
  , FromJSON agg, ToJSON agg                   -- ^ Aggregates are stored in JSON so need to be deserialized.
  , FromJSON (Events agg), ToJSON (Events agg) -- ^ Encode and decode events as well for processing.
  , Project agg                                -- ^ Once decoded, we must be able to apply events to it.
  , Typeable agg                               -- ^ We need to encode the aggregate type in the DB.
  , Typeable (Events agg)                      -- ^ We also need to encode the type of event in the DB.
  )


-- Run a command by executing the wrapping writer and reader monads
-- around a command, and then applying the resulting events to latest
-- snapshot that has been loaded.
runCommand
  :: CommandContext agg m      -- ^ `CommandContext`
  => UUID                      -- ^ Which aggregate do we care about.
  -> Command agg m             -- ^ A Command that emits events.
  -> m ()

runCommand uuid m = do
  -- Use the backend to fetch a current aggregate, and produce a new
  -- event list from the passed command.
  backend           <- ask
  agg@Aggregate{..} <- loadAggregate backend uuid
  result            <- snd <$> runWriterT (flip runReaderT value m)

  -- For each event we've mapped, we want to encode and snapshot the
  -- result in our backing store.
  for_ (snd $ foldEvents agg result) $ \(version, agg, event) -> do
    let encodedEvent = Event
          { uuid      = uuid
          , kind      = show (typeOf agg)
          , emitted   = "NOW"
          , version   = version
          , eventName = show (typeOf event)
          , eventBody = toStrict (encodeToLazyText event)
          , snapshot  = toStrict (encodeToLazyText agg)
          }

    print encodedEvent

  where
    -- Pair up each event with a snapshot of its current state, so that
    -- we can persist a snapshot log along with the event.
    foldEvents = mapAccumL $ \Aggregate{..} event ->
      let version = currentVersion + 1 in
      let updated = foldEvent value event in
      (Aggregate updated version, (version, updated, event))
