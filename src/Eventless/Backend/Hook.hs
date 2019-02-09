module Eventless.Backend.Hook
  ( hookMiddleware
  )
where

--------------------------------------------------------------------------------

import           Protolude

import           Data.Aeson                     ( FromJSON, decode )
import           Data.UUID                      ( UUID, toText )
import           Eventless                      ( Aggregate(..)
                                                , BackendStore(..)
                                                , Event(..)
                                                )

--------------------------------------------------------------------------------

type CommitHook =
  forall m. MonadIO m
    => UUID
    -> Event
    -> m ()

hookMiddleware
  :: CommitHook
  -> BackendStore
  -> BackendStore

hookMiddleware callback backend = Backend
  { loadLatest            = loadLatest backend
  , loadVersion           = loadVersion backend
  , writeEventTransaction = \uuid events -> do
      writeEventTransaction backend uuid events
      let final = getLast $ foldMapDefault (Last . Just) events
      case final of
        Nothing    -> pure ()
        Just event -> callback uuid event
  }
