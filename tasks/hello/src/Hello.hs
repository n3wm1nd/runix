module Hello where

import Runix.Effects
import Polysemy

helloworld :: Member Logging r => Sem r ()
helloworld = do
  info "hello world"
