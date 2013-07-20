---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

--import qualified Data.Graph.Inductive as DG
--import qualified System.Directory     as System.Directory

----import Control.Monad.State
----import Data.Graph.Inductive.Tree
----import Data.Graph.Inductive.Monad
----import Data.Graph.Inductive.Monad.IOArray

--import qualified Luna.DefManager as DefManager
--import           Luna.DefManager   (DefManager)
--import qualified Luna.Library as Library
--import qualified Luna.System.UniPath as Path


--import qualified Luna.Graph as Graph
--import qualified Luna.Node                as Node
--import           Luna.Node                  (Node)
--import qualified Luna.NodeDef             as NodeDef
--import qualified Luna.Samples             as Samples
--import qualified Luna.Tools.CodeGenerator as CG
--import qualified Luna.Tools.Graphviz      as Graphviz
--import qualified Luna.Tools.TypeChecker as TC

--import Text.Show.Pretty
--import Text.Groom

import Luna.Lib.Library (Library(..))
import qualified Luna.Lib.LibManager as LibManager
import qualified Luna.System.UniPath as UniPath

import qualified Luna.Network.Def.NodeDef as NodeDef
import qualified Luna.Type.Type           as Type
import qualified Luna.Network.Def.DefManager as DefManager


main :: IO ()
main = do 
    let
        stdlib = Library $ UniPath.fromUnixString "/opt/flowbox/luna/stdlib"
        libman = LibManager.register (0, stdlib)
               $ LibManager.empty

        n_std   = NodeDef.empty $ Type.Package "std"
        manager = DefManager.add (0, n_std)
        	    $ DefManager.empty
        
        
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
--    	let 
--            lib = Library.Library $ Path.fromUnixString "lunalib/std"
--        putStrLn "Hello programmer! I am Lunac, the Luna compiler"
--        pwd <- System.Directory.getCurrentDirectory
--        putStrLn $ "My PWD is " ++ pwd
--        print "Original manager:"
--        print $ snd Samples.sample_helloWorld
--        print "====================================="
--        print "load.save :"
--        DefManager.saveManager (Path.fromUnixString "lunalib") $ snd Samples.sample_helloWorld
--    	manager <- DefManager.load lib DefManager.empty
--    	print manager
        

--showCode :: Node -> DefManager -> IO ()
--showCode node manager = putStrLn $ CG.generateCode node manager

