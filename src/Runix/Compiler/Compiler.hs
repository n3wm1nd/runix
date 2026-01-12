{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}



module Runix.Compiler.Compiler
    (
        compile
    ) where

import Runix.Compiler
import qualified Runix.Compiler.Cabal as CC
import qualified Distribution.ModuleName

compile :: HaskellProject -> IO CompileResult
compile project = do
    let modname = Distribution.ModuleName.fromString project.main.modulename
    let modcode = project.main.sourcecode
    let proj = CC.emptyProject { 
        CC.mainmodule = CC.ModuleSource modname modcode
    }
    res <- CC.compile proj
    return $ case res of
        -- FIXME correctly parse and translate warnings and errors
        CC.CompileError {} -> do
            CompileFail [] []
        CC.CompileSuccess {} -> do
            CompileSuccess []