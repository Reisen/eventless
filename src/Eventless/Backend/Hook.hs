module Eventless.Backend.Hook
  ( hookMiddleware
  ) where

import Protolude
import Data.UUID  ( UUID )
import Eventless  ( BackendStore (..), Event (..) )

--------------------------------------------------------------------------------

type CommitHook =
  forall m t. MonadIO m
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

  , loadEvents            = \uuid -> do
      events <- loadEvents backend uuid
      callback uuid events
      pure events

  , writeEventTransaction = \uuid events -> do
      writeEventTransaction backend uuid events
      callback uuid events
  }
