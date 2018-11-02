module Eventless.Types.Event
  ( Event     (..)
  , Events    (..)
  , Eventless
  , Project   (..)
  , UUID
  ) where


import Protolude hiding (MonadReader)
import Data.Aeson       (ToJSON)
import Control.Monad.Classes


-- Aggregates UUID's are GUUID's as far as possible. We
-- fix on a specific type so that this remains consistent
-- accross the entire identity store. This also cuts down
-- on abuse of ID type in event sourced systems.
type UUID = Text


-- The `Event` type represents the encoded resulting
-- Event produced by a system. This is the type that we
-- serialize into various backends.
data Event = Event
  { kind      :: Text
  , emitted   :: Text
  , version   :: Int
  , eventName :: Text
  , eventBody :: LText
  , snapshot  :: LText
  } deriving Show


-- All aggregates are modified by a single event kind, that type must be
-- associated with it via some mechanism. To achieve thsi we use an injective
-- type family to calculate the unique event type at the type level.
type family Events (a :: Type) = r | r -> a


-- Any monad that provides a way to both access the _current_ snapshot of an
-- aggregate, and log events that affect it, is a viable Eventless monad
-- context to define a DSL within.
type Eventless a m =
  ( MonadWriter [Events a] m
  , MonadReader a m
  )


-- Foldable Events
class Project agg where
  foldEvent
    :: agg
    -> Events agg
    -> agg
