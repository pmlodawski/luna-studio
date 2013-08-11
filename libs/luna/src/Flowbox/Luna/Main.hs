---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

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

--import qualified Flowbox.Luna.Network.Graph.Graph      as Graph

import qualified Flowbox.Luna.Data.Graph               as Graph
import qualified Flowbox.Luna.Lib.Library              as Library


test manager did = out where
    mod = DG.generateDefinition manager did
    out = trace(show mod) mod

main :: IO ()
main = do 
    putStrLn "------------\n"
    putStrLn $ Module.genCode $ DG.generateDefinition HelloWorld.full_manager 100
    --putStrLn $ Module.genCode $ CG.generateCommonCls "select0"
    --putStrLn


    --let
    --    manager = HelloWorld.full_manager
    --    x = map (test manager)$ DefManager.nodes manager

    --Library.store HelloWorld.workspacelib
    return ()

--        let 
--                (node, manager) = Samples.sample_helloWorld
--                nodeDef = Node.def node
--                graph = NodeDef.graph nodeDef
--        Graphviz.showGraph graph
--        print $ show $ TC.typeCheck graph manager
--        showCode node manager
--        putStrLn "=================================="
--        testSerialization
--        return ()


--testSerialization = do
--      let 
--            lib = Library.Library $ Path.fromUnixString "lunalib/std"
--        putStrLn "Hello programmer! I am Lunac, the Luna compiler"
--        pwd <- System.Directory.getCurrentDirectory
--        putStrLn $ "My PWD is " ++ pwd
--        print "Original manager:"
--        print $ snd Samples.sample_helloWorld
--        print "====================================="
--        print "load.save :"
--        DefManager.saveManager (Path.fromUnixString "lunalib") $ snd Samples.sample_helloWorld
--      manager <- DefManager.load lib DefManager.empty
--      print manager
        

--showCode :: Node -> DefManager -> IO ()
--showCode node manager = putStrLn $ CG.generateCode node manager

