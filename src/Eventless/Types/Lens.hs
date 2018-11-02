module Eventless.Types.Lens
  ( kind
  , emitted
  , version
  , eventName
  , snapshot
  ) where


import Protolude
import qualified Eventless.Types.Event as Event


kind :: Functor f => (Text -> f Text) -> (Event.Event -> f Event.Event)
kind l e = (\v -> e { Event.kind = v }) <$> l (Event.kind e)

emitted :: Functor f => (Text -> f Text) -> (Event.Event -> f Event.Event)
emitted l e = (\v -> e { Event.emitted = v }) <$> l (Event.emitted e)

version :: Functor f => (Int -> f Int) -> (Event.Event -> f Event.Event)
version l e = (\v -> e { Event.version = v }) <$> l (Event.version e)

eventName :: Functor f => (Text -> f Text) -> (Event.Event -> f Event.Event)
eventName l e = (\v -> e { Event.eventName = v }) <$> l (Event.eventName e)

eventBody :: Functor f => (LText -> f LText) -> (Event.Event -> f Event.Event)
eventBody l e = (\v -> e { Event.eventBody = v }) <$> l (Event.eventBody e)

snapshot :: Functor f => (LText -> f LText) -> (Event.Event -> f Event.Event)
snapshot l e = (\v -> e { Event.snapshot = v }) <$> l (Event.snapshot e)

