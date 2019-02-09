module Eventless.Commands
  ( emit
  , loadSnapshot
  , runCommand
  )
where

--------------------------------------------------------------------------------

import           Protolude
import           Eventless.Types.Aggregate
import           Eventless.Types.BackendStore
import           Eventless.Types.Event

import           Control.Monad.Writer           ( MonadWriter(..) )
import           Control.Monad.Writer.Lazy      ( WriterT, runWriterT )
import           Data.Aeson                     ( FromJSON, ToJSON )
import           Data.Aeson.Text                ( encodeToLazyText )
import           Data.Data                      ( Data, toConstr )
import           Data.Default.Class             ( Default, def )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Typeable                  ( Typeable, typeOf )
import           Data.UUID                      ( UUID )

--------------------------------------------------------------------------------

-- | An actual command is just a function that has access to the current
-- | aggregate state and produces events based on this. In other words we just
-- | run in the Eventless context.
-- |
-- | TODO: Use something more efficient than a list for event aggregation.
type Command agg m = ReaderT (Maybe agg) (WriterT [Events agg] m) ()

--------------------------------------------------------------------------------

-- | Perform an Event fold. We thread the aggregate state through the folding
-- | operation so we can store snapshots at each stage.
foldEvents
  :: Project agg
  => Traversable t
  => Aggregate agg                                  -- ^ A starting state to work from.
  -> t (Events agg)                                 -- ^ A list of events to apply/project.
  -> (Aggregate agg, t (Events agg, Aggregate agg)) -- ^ A pair of the final result and all the intermediate states.

foldEvents =
  mapAccumL $ \Aggregate {..} event -> (\agg -> (,) agg (event, agg)) Aggregate
    { aggregateUUID    = aggregateUUID
    , aggregateValue   = foldEvent aggregateValue event
    , aggregateVersion = aggregateVersion + 1
    }

--------------------------------------------------------------------------------

type CommandContext agg m =
  ( Data (Events agg)        -- ^ Allows us to use toConstr to get the name of our event.
  , Default agg              -- ^ First Event requires a default to project.
  , FromJSON (Events agg)    -- ^ Allows us to decode events.
  , FromJSON agg             -- ^ Allows us to decode snapshots (for projecting).
  , MonadIO m                -- ^ Allows us to use our backends.
  , Project agg              -- ^ Allows us to fold events.
  , ToJSON (Events agg)      -- ^ Allows us to encode events.
  , ToJSON agg               -- ^ Allows us to encode snapshots.
  , Typeable agg             -- ^ Allows us to get at the name of our aggregate.
  )

-- | Run a command by executing the wrapping writer and reader monads around a
-- | command, and then applying the resulting events to the latest snapshot that
-- | has been loaded.
-- |
-- | TODO: Offer a way to handle failure.
-- | TODO: Move away from using Default and provide a real mechanism for initialState.
-- | TODO: Cull all these constraints.
runCommand
  :: CommandContext agg m
  => BackendStore             -- ^ A Backend context used to read/write events to.
  -> UUID                     -- ^ Which aggregate do we care about.
  -> Command agg m            -- ^ A Command that emits events.
  -> m (Aggregate agg)        -- ^ Return the resulting aggregate.

runCommand backend uuid m = do
  -- Use the backend to fetch a current aggregate, and produce a new event list
  -- from the passed command.
  emittedAt <- liftIO getCurrentTime
  agg       <- loadLatest backend uuid
  result    <- map snd . runWriterT . runReaderT m $ aggregateValue <$> agg

  -- Use either the current aggregate state or a default.
  let initialState = fromMaybe (Aggregate uuid def 0) agg

  -- Run the events over the aggregate.
  let foldedEvents = foldEvents initialState result

  -- Extract the final state and events that lead to it.
  let currentState = fst foldedEvents
  let resultEvents = snd foldedEvents

  -- Encode the resulting events to be written to the DB.
  let encodeEvents = flip map resultEvents $ \(event, Aggregate {..}) -> Event
        { eventKind     = show (typeOf aggregateValue)
        , eventEmitted  = show emittedAt
        , eventVersion  = aggregateVersion
        , eventName     = show (toConstr event)
        , eventBody     = encodeToLazyText event
        , eventSnapshot = encodeToLazyText aggregateValue
        }

  writeEventTransaction backend uuid encodeEvents
  pure currentState

--------------------------------------------------------------------------------

-- | We are using the Writer monad as a way of collecting emited events, so we
-- | can re-use the operation. We just rename it to make what we're doing more
-- | intuitive.
emit :: MonadWriter [a] m => a -> m ()
emit v = tell [v]

-- | Similar to emit, we're using the Reader monad to inject the current working
-- | state during a command. Renamed just for intuition.
loadSnapshot :: MonadReader r m => m r
loadSnapshot = ask
