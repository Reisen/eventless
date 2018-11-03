module Eventless.Types.BackendStore
  ( BackendStore (..)
  ) where


import Protolude
import Data.Aeson (FromJSON, ToJSON)
import Control.Monad.Classes

import Eventless.Types.Aggregate
import Eventless.Types.Event


data BackendStore = Backend
  { loadAggregate
      :: forall a m.
         MonadExec IO m
      => FromJSON a
      => UUID
      -> m (Maybe (Aggregate a))

  , writeEvents
      :: forall m t.
         MonadExec IO m
      => Traversable t
      => UUID
      -> t Event
      -> m ()
  }
