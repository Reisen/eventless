module Eventless.Types.Lens
  ( HasVersion (..)

  -- Event Lenses
  , kind
  , emitted
  , name
  , body
  , snapshot

  -- Aggregate Lenses
  , uuid
  , value
  ) where

import Protolude
import Eventless.Types.Aggregate
import Eventless.Types.Event



-- Create Polymorphic Lens for Version
--
class HasVersion s a | s -> a where
  version :: Functor f => (a -> f a) -> (s -> f s)



-- Create Lenses for Event Fields
--
type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f
  => (a -> f b)
  -> (s -> f t)

kind :: Lens' Event Text
kind l e = (\v -> e { eventKind = v }) <$> l (eventKind e)

emitted :: Lens' Event Text
emitted l e = (\v -> e { eventEmitted = v }) <$> l (eventEmitted e)

instance HasVersion Event Int where
  version l e = (\v -> e { eventVersion = v }) <$> l (eventVersion e)

name :: Lens' Event Text
name l e = (\v -> e { eventName = v }) <$> l (eventName e)

body :: Lens' Event LText
body l e = (\v -> e { eventBody = v }) <$> l (eventBody e)

snapshot :: Lens' Event LText
snapshot l e = (\v -> e { eventSnapshot = v }) <$> l (eventSnapshot e)



-- Create Lenses for Aggregate Fields
--
uuid :: Lens' (Aggregate a) UUID
uuid l e = (\v -> e { aggregateUUID = v }) <$> l (aggregateUUID e)

value :: Lens' (Aggregate a) a
value l e = (\v -> e { aggregateValue = v }) <$> l (aggregateValue e)

instance HasVersion (Aggregate a) Int where
  version l e = (\v -> e { aggregateVersion = v }) <$> l (aggregateVersion e)
