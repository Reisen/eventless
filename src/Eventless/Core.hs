module Eventless.Core
  ( module Aggregate
  , module BackendStore
  , module Commands
  , module Event
  , emit
  , loadSnapshot
  ) where

import Data.Aeson                (FromJSON, ToJSON)
import Data.Sequence             (Seq)
import Data.Text.Lazy            (toStrict)
import Control.Monad.Classes
import Protolude hiding
  ( MonadReader
  , ask
  )

import Eventless.Types.Aggregate    as Aggregate
import Eventless.Types.BackendStore as BackendStore
import Eventless.Types.Commands     as Commands
import Eventless.Types.Event        as Event




-- Emit events into a log.
emit :: MonadWriter [a] m => a -> m ()
emit v = tell [v]


-- View the current snapshot of the aggregate we are modifying.
loadSnapshot :: MonadReader r m => m r
loadSnapshot = ask
