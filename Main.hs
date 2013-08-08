---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Flowbox.Luna.Core                        as Core
import           Flowbox.Luna.Core                          (Core(..))
import qualified Flowbox.Luna.Lib.LibManager              as LibManager
import qualified Flowbox.Luna.Samples.HelloWorld          as HelloWorld
import qualified Flowbox.Luna.Tools.Serialize.Serializer  as Serializer
import qualified Flowbox.Luna.Tools.Serialize.Lib         as LibSerializer
import qualified Flowbox.Luna.Samples.HelloWorld          as HelloWorld
import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator    as FG
import qualified Flowbox.Luna.Codegen.Hs.ModGenerator     as MG
import qualified Flowbox.Luna.Network.Def.DefManager      as DefManager

import qualified Flowbox.Luna.Codegen.Hs.AST.Function     as Function
import qualified Flowbox.Luna.Codegen.Hs.AST.Module       as Module

import Flowbox.Luna.Data.Graph


main :: IO ()

main = do 
    let libManager = HelloWorld.base_libman
        project = Core libManager HelloWorld.full_manager
        Just stdLib = LibManager.lab libManager 0 
    print project
    putStrLn "------------\n"

    LibSerializer.storeLib project stdLib

    --project2 <- Serializer.restoreLib project stdLib
    --print project2
    --putStrLn $ FG.generateFunction HelloWorld.myFun3
    --putStrLn $ MG.generateDefinition HelloWorld.full_manager 1
    --print $ MG.generateModule HelloWorld.full_manager 100
    --putStrLn $ Module.genCode $ MG.generateDefinition HelloWorld.full_manager 1
    --print $ FG.generateFunction HelloWorld.myFun3
    --putStrLn $ Function.genCode GenContext.empty $ FG.generateFunction HelloWorld.myFun3

    --putStrLn $ Cg.generateDefCode $
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

