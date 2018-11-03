module Eventless.Commands
  ( runCommand
  , emit
  , loadSnapshot
  ) where


import Control.Monad.Classes
import Control.Monad.Writer.Lazy (WriterT, runWriterT)
import Data.Aeson                (FromJSON, ToJSON)
import Data.Aeson.Text           (encodeToLazyText)
import Data.Data                 (Data, toConstr)
import Data.Default.Class        (Default, def)
import Data.Time.Clock           (getCurrentTime)
import Data.Typeable             (Typeable, typeOf)
import Protolude hiding
  ( MonadReader
  , ask
  )

import Eventless.Types.Aggregate
import Eventless.Types.BackendStore
import Eventless.Types.Event


-- An actual command is just a function that has access to the current
-- aggregate state and produces events based on this. In other words we just
-- run in the Eventless context.
type Command agg m = ReaderT (Maybe agg) (WriterT [Events agg] m) ()


-- Perform an Event fold. We thread the aggregate state through the folding
-- operation so we can store snapshots at each stage.
foldEvents
  :: Project agg
  => Traversable t
  => Aggregate agg
  -> t (Events agg)
  -> t (Events agg, Aggregate agg)

foldEvents = (fmap . fmap $ snd) $ mapAccumL $ \Aggregate{..} event ->
  (\agg -> (,) agg (event, agg)) Aggregate
    { uuid  = uuid
    , value = foldEvent value event
    , currentVersion = currentVersion + 1
    }


type ProjectionContext agg m =
  ( Default agg
  , Project agg
  , Typeable agg
  , MonadExec IO m
  , Data (Events agg)
  , Typeable (Events agg)
  , FromJSON agg, ToJSON agg
  , FromJSON (Events agg), ToJSON (Events agg)
  )

-- Run a command by executing the wrapping writer and reader monads around a
-- command, and then applying the resulting events to the latest snapshot that
-- has been loaded.
runCommand
  :: ProjectionContext agg m   -- ^ ProjectionContext
  => BackendStore              -- ^ A Backend context used to read/write events to.
  -> UUID                      -- ^ Which aggregate do we care about.
  -> Command agg m             -- ^ A Command that emits events.
  -> m ()

runCommand backend uuid m = do
  -- Use the backend to fetch a current aggregate, and produce a new event list
  -- from the passed command.
  agg       <- loadAggregate backend uuid
  result    <- snd <$> runWriterT (flip runReaderT (value <$> agg) m)
  emittedAt <- exec $ getCurrentTime

  -- For each event we've mapped, we want to encode and snapshot the
  -- result in our backing store.
  let initialState = fromMaybe (Aggregate uuid def 0) agg
  let foldedEvents = foldEvents initialState result
  let encodeEvents = flip map foldedEvents $ \(event, Aggregate{..}) ->
        Event
          { kind      = show (typeOf value)
          , emitted   = show emittedAt
          , version   = currentVersion
          , eventName = show (toConstr event)
          , eventBody = encodeToLazyText event
          , snapshot  = encodeToLazyText value
          }

  writeEvents backend uuid encodeEvents


-- Emit events into a log.
emit :: MonadWriter [a] m => a -> m ()
emit v = tell [v]


-- View the current snapshot of the aggregate we are modifying.
loadSnapshot :: MonadReader r m => m r
loadSnapshot = ask
