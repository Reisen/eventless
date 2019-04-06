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
  { loadAggregates        = loadAggregates backend
  , loadEvents            = loadEvents backend
  , loadLatest            = loadLatest backend
  , loadVersion           = loadVersion backend
  , writeEventTransaction = hookWriteEventTransaction callback backend
  }


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
