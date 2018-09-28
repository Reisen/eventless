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
      :: forall a m. MonadExec IO m
      => FromJSON a
      => UUID
      -> m (Aggregate a)

  , writeEvents
      :: forall agg event m. MonadExec IO m
      => ToJSON event
      => ToJSON agg
      => UUID
      -> [(agg, event)]
      -> m ()
  }
