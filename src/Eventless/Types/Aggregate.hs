module Eventless.Types.Aggregate
  ( Aggregate (..)
  ) where


import Protolude
import Eventless.Types.Event (UUID)


-- This wrapper is used when extracting aggregates from the database,
-- it allows requesting the value itself as well as metrics about the
-- value such as its current version.
data Aggregate a = Aggregate
  { aggregateUUID    :: UUID
  , aggregateValue   :: a
  , aggregateVersion :: Int
  } deriving Show
