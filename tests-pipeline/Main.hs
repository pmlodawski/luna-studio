{-# LANGUAGE LambdaCase #-}

module Main where


import Flowbox.Prelude

import System.Environment                                                       (getArgs)

import Text.PrettyPrint                                                         (render)
import Test.Hspec.LunaTypechecker
import Test.Hspec.LunaTypechecker.CompilerPipeline.CompilerPipelineProgressStep (CompilerPipelineProgressStep(..))



main :: IO ()
main = do
    getArgs >>= \case
        [filename] -> do
            result <- lunaCompilerStepsFile filename
            let output = prettyCompilerPipelineResult displayResults result
            putStrLn (render output)
        _ -> print ( "invalid number of arguments: pass (only) the filename" :: String )

  where
    displayResults = [D_DesugarImplicitSelf_AST, F_TypecheckerInference_AST, F_TypecheckerInference_TCState]