module Eventless.Types.BackendStore
  ( BackendStore (..)
  ) where


import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Eventless.Types.Aggregate
import Eventless.Types.Event


data BackendStore = Backend
  { loadAggregate
      :: forall a m.
         MonadIO m
      => FromJSON a
      => UUID
      -> m (Maybe (Aggregate a))

  , writeEvents
      :: forall m t.
         MonadIO m
      => Traversable t
      => UUID
      -> t Event
      -> m ()
  }
