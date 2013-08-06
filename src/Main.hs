---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Luna.Core                      as Core
import           Luna.Core                        (Core(..))
import qualified Luna.Codegen.FuncGenerator     as FG
import qualified Luna.Codegen.DefGenerator      as DG
import qualified Luna.Lib.LibManager            as LibManager
import qualified Luna.Samples.HelloWorld        as HelloWorld
import qualified Luna.Tools.Serializer          as Serializer

main :: IO ()
main = do 
    let libManager = HelloWorld.base_libman
        core = Core libManager HelloWorld.full_manager
        Just stdLib = LibManager.lab libManager 0 
    print core
    putStrLn "------------\n"

    Serializer.storeLib core stdLib

    core2 <- Serializer.restoreLib core stdLib
	--putStrLn $ FG.generateFunction HelloWorld.myFun3
    --putStrLn $ DG.generateDefinition 1 HelloWorld.full_manager
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

