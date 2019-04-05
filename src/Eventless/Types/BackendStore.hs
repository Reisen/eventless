module Eventless.Types.BackendStore
  ( BackendStore(..)
  ) where

import Protolude
import Data.Aeson                ( FromJSON )
import Data.UUID                 ( UUID )
import Eventless.Types.Aggregate
import Eventless.Types.Event

--------------------------------------------------------------------------------

-- | A BackendStore wraps the API that reads/writes to the underlying event
-- | store. We use a standard datatype here instead of a class so that these
-- | can be more easily stored and moved around code-bases. It also implies no
-- | requirement on consumers to use a new monad transformer or something
-- | similar.
data BackendStore = Backend
  { loadLatest            :: LoadLatest
  , loadVersion           :: LoadVersion
  , loadEvents            :: LoadEvents
  , loadAggregates        :: LoadAggregates
  , writeEventTransaction :: WriteEventTransaction
  }


-- | Loads the latest version of an event and aggregate by UUID, can fail.
type LoadLatest
  =  forall a m. MonadIO m
  => FromJSON a
  => UUID
  -> m (Maybe (Aggregate a))

-- | Load a specific version of an event and aggregate of a specific UUID.
type LoadVersion
  =  forall a m. MonadIO m
  => FromJSON a
  => UUID
  -> Int
  -> m (Maybe (Aggregate a))

-- | Load all Events for an Aggregate ID.
type LoadEvents
  =  forall m. MonadIO m
  => UUID
  -> m [Event]

-- | Load all Aggregate UUIDs.
type LoadAggregates
  =  forall m. MonadIO m
  => m [UUID]

-- | Write all events into the backend store in a single transaction (can fail).
type WriteEventTransaction
  =  forall m t. MonadIO m
  => Traversable t
  => UUID
  -> t Event
  -> m ()
