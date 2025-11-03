{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for UniversalLLM tool system with Polysemy's Sem monad.
--
-- These instances allow Polysemy effects to work seamlessly with the UniversalLLM
-- tool calling system. They are orphan instances because:
-- - We can't add them to universal-llm without adding a polysemy dependency
-- - We can't add them to polysemy (it's external)
-- - They need to live in the Runix codebase which uses both libraries
module Runix.LLM.ToolInstances where

import Polysemy (Sem)
import Data.Proxy (Proxy(..))
import UniversalLLM.Core.Tools
  ( Callable(..)
  , Tool(..)
  , ToolFunction(..)
  , ToolParameter(..)
  )

-- | Callable instance for 0-arity Sem actions (base case for recursion)
-- This allows bare Sem r a actions to be used as tools when 'a' has a ToolFunction instance
instance ToolParameter a => Callable (Sem r a) (Sem r) () a where
  call action () = action

-- | Tool instance for 0-arity Sem actions
-- This extracts the tool name and description from the result type's ToolFunction instance
instance ToolFunction a => Tool (Sem r a) (Sem r) () a where
  toolName _ = toolFunctionName (Proxy @a)
  toolDescription _ = toolFunctionDescription (Proxy @a)
