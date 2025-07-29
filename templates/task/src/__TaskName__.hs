module __TaskName__ where

import Runix.Effects
import Polysemy

-- | Main entry point for the __taskName__ task
__taskName__ :: Member Logging r => Sem r ()
__taskName__ = do
  info "Starting __taskName__ task"
  -- TODO: Implement your task logic here
  info "__taskName__ task completed"
