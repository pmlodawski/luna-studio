---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Flowbox.Luna.Samples.HelloWorld        as HelloWorld
import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator  as FG
import qualified Flowbox.Luna.Codegen.Hs.DefGenerator   as DG
import qualified Flowbox.Luna.Codegen.Hs.CodeGenerator  as CG
import qualified Flowbox.Luna.Network.Def.DefManager    as DefManager

import qualified Flowbox.Luna.Codegen.Hs.AST.Function   as Function
import qualified Flowbox.Luna.Codegen.Hs.AST.Module     as Module

import           Flowbox.Luna.Codegen.Hs.Cabal.Config      (Config)
import qualified Flowbox.Luna.Codegen.Hs.Cabal.Config    as Config
import qualified Flowbox.Luna.Codegen.Hs.Cabal.Section   as Section

import qualified Flowbox.Luna.Network.Graph.Graph              as Graph

import Flowbox.Luna.Data.Graph


main :: IO ()

main = do 
    putStrLn "------------\n"

    --putStrLn $ Module.genCode $ DG.generateDefinition HelloWorld.full_manager 1
    --putStrLn $ Module.genCode $ CG.generateCommonCls "select0"

    --let
    --	s = Section.empty 
    --	c = Config.addSection s
    --		Config.empty { Config.name = "ala"}

    --putStrLn $ Config.genCode c

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

