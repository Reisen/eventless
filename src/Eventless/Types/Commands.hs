module Eventless.Types.Commands
  ( Command
  ) where


import Protolude
import Control.Monad.Writer.Lazy (WriterT, runWriterT)
import Eventless.Types.Event


-- An actual command is just a function that has access to the current
-- aggregate state and produces events based on this. In other words we just
-- run in the Eventless context.
type Command agg m = ReaderT (Maybe agg) (WriterT [Events agg] m) ()
