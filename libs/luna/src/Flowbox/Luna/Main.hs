---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Debug.Trace                             

import qualified Flowbox.Luna.Samples.HelloWorld       as HelloWorld
import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator as FG
import qualified Flowbox.Luna.Codegen.Hs.DefGenerator  as DG
import qualified Flowbox.Luna.Codegen.Hs.CodeGenerator as CG
import qualified Flowbox.Luna.Network.Def.DefManager   as DefManager

import qualified Flowbox.Luna.Codegen.Hs.AST.Function  as Function
import qualified Flowbox.Luna.Codegen.Hs.AST.Module    as Module

import           Flowbox.Luna.Codegen.Hs.Cabal.Config    (Config)
import qualified Flowbox.Luna.Codegen.Hs.Cabal.Config  as Config
import qualified Flowbox.Luna.Codegen.Hs.Cabal.Section as Section

import qualified Flowbox.Luna.Data.Graph               as Graph
import qualified Flowbox.Luna.Lib.Library              as Library

import qualified Flowbox.Luna.Builder.Builder          as Builder
import qualified Flowbox.System.UniPath                as UniPath
import           Flowbox.System.UniPath                  (UniPath)
import qualified Flowbox.Luna.Parser.Parser            as Parser
import qualified Flowbox.Luna.Parser.Lexer             as Lexer


test :: DefManager.DefManager -> Graph.Vertex -> Module.Module
test manager did = out where
    mod = DG.generateDefinition manager did
    out = trace(show mod) mod


main :: IO ()
main = do 
    Parser.main
    --putStrLn "------------\n"
    --putStrLn $ Module.genCode $ DG.generateDefinition HelloWorld.full_manager 100
    --putStrLn $ Module.genCode $ CG.generateCommonCls "select0"


    --let
    --    builder = Builder.empty { Builder.path = UniPath.fromUnixString("samples/TestProject/build") }
    --Builder.buildLibrary builder (HelloWorld.workspacelib)
    --return ()

