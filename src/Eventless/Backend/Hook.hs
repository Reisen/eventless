module Eventless.Backend.Hook
  ( hookMiddleware
  ) where

import Protolude
import Data.UUID  ( UUID )
import Eventless  ( BackendStore (..), Event (..) )

--------------------------------------------------------------------------------

type CommitHook =
  forall m t. MonadIO m
    => Traversable t
    => UUID
    -> t Event
    -> m ()

hookMiddleware
  :: CommitHook
  -> BackendStore
  -> BackendStore

hookMiddleware callback backend = Backend
  { loadLatest            = loadLatest backend
  , loadVersion           = loadVersion backend
  , loadAggregates        = loadAggregates backend
  , loadEvents            = hookLoadEvents callback backend
  , writeEventTransaction = hookWriteEventTransaction callback backend
  }


hookLoadEvents
  :: MonadIO m
  => CommitHook
  -> BackendStore
  -> UUID
  -> m [Event]

hookLoadEvents callback backend uuid = do
  events <- loadEvents backend uuid
  callback uuid events
  pure events


hookWriteEventTransaction
  :: MonadIO m
  => Traversable t
  => CommitHook
  -> BackendStore
  -> UUID
  -> t Event
  -> m ()

hookWriteEventTransaction callback backend uuid events = do
  writeEventTransaction backend uuid events
  callback uuid events
