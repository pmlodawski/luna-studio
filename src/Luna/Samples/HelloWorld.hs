---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Samples.HelloWorld(
    workspaceImports,
    base_workspace,
    base_manager,
    full_manager,
    myFun, myFun2
) where


import qualified Data.Map                        as Map
import qualified Luna.Network.Flags              as Flags
import qualified Luna.Network.Attributes         as Attributes
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager(..))
import qualified Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Luna.Network.Graph.Edge         as Edge
import           Luna.Network.Graph.Edge           (Edge(..))
import qualified Luna.Network.Graph.Graph        as Graph
import qualified Luna.Network.Graph.Node         as Node
import           Luna.Network.Graph.Node           (Node)
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef(..))
import           Luna.Lib.Library                  (Library(..))
import qualified Luna.Lib.LibManager             as LibManager
import           Luna.Lib.LibManager               (LibManager(..))
import qualified Luna.System.UniPath             as UniPath
import qualified Luna.Type.Type                  as Type
import           Luna.Type.Type                    (Type(..))
import           Luna.Network.Path.Path            (Path(..))
import           Luna.Network.Path.Import          (Import(..))


stdLibKey = 0
userLibKey = 1

workspaceImports = [ Import (Path ["Std","IO","Console"]) ["Init", "Print"]
                   , Import (Path ["Std","Types"])        ["Type", "New", "String"]
                   ]

base_workspace :: Type
base_workspace = Type.Package "__workspace__" workspaceImports

base_manager :: DefManager
base_manager = manager where
    manager = DefManager.add (100, NodeDef.make base_workspace userLibKey )
            $ DefManager.empty 


base_libman :: LibManager
base_libman = libman where
    libman = LibManager.register (userLibKey, Library "__workspace__" $ UniPath.fromUnixString "~/flowbox/project1")
           $ LibManager.register (stdLibKey,  Library "std"           $ UniPath.fromUnixString "/opt/flowbox/luna/stdlib")
           $ LibManager.empty {defManager = base_manager }




--sample :: LibManager
--sample = libman where

--    ---- user generated ------------------------------------------------------------------------------------------------------------



    

    
myFunGraph = Graph.connectMany [(0, 1, Edge 0 0 Edge.Standard),
                                (2, 4, Edge 0 0 Edge.Standard),
                                (3, 4, Edge 0 0 Edge.Standard),
                                (1, 5, Edge 0 0 Edge.Standard),
                                (4, 5, Edge 0 1 Edge.Standard)]

           $ Graph.addMany [(0, Node.Type "Console" Flags.empty Attributes.empty),
                            (1, Node.Call "init" Flags.empty Attributes.empty),
                            (2, Node.Type "String" Flags.empty Attributes.empty),
                            (3, Node.Default $ DefaultValue.DefaultString "hello world!"),
                            (4, Node.Call "init" Flags.empty Attributes.empty),
                            (5, Node.Call "print" Flags.empty Attributes.empty)]
           $ Graph.empty

myFunInputs = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]

myFun = NodeDef (Type.Function "myFun" myFunInputs Type.noOutputs) 
                 myFunGraph 
                 Flags.empty 
                 Attributes.empty 
                 userLibKey



myFunGraph2 = Graph.connectMany [(0, 2, Edge 0 0 Edge.Standard),
                                 (0, 2, Edge 1 1 Edge.Standard),
                                 (2, 1, Edge 0 0 Edge.Standard)
                                ]

           $ Graph.addMany [(0, Node.Inputs Flags.empty Attributes.empty),
                            (1, Node.Outputs Flags.empty Attributes.empty),
                            (2, Node.Call "add" Flags.empty Attributes.empty)
                            ]
           $ Graph.empty

myFunInputs2 = Type.Tuple [Type.TypeVariable "a", 
                           Type.Named "in1" $ Type.TypeVariable "b"]

myFun2 = NodeDef (Type.Function "myFun2" myFunInputs2 Type.noOutputs) 
                 myFunGraph2 
                 Flags.empty 
                 Attributes.empty 
                 userLibKey





--full_manager :: DefManager
--full_manager =  DefManager.addToParentMany [
--                         (8, 9, NodeDef.make (Type.mkPackage "Init") stdLibKey),
--                     (5, 8, NodeDef.make (Type.mkPackage "String") stdLibKey),
--                     (5, 7, NodeDef.make (Type.mkPackage "Type") stdLibKey),
--                     (5, 6, NodeDef.make (Type.mkPackage "New") stdLibKey),
--                 (0, 5, NodeDef.make (Type.mkPackage "Types") stdLibKey),
--                         (2, 4, NodeDef.make (Type.Function "Print" [Type.Class "Console" [], Type.Class "String" []] [Type.Class "Console"[]]) stdLibKey),
--                         (2, 3, NodeDef.make (Type.Function "Init" [Type.Class "Console" []] [Type.Class "Console"[]]) stdLibKey),
--                     (1, 2, NodeDef.make (Type.Class "Console" []) stdLibKey),
--                 (0, 1, NodeDef.make (Type.mkPackage "IO") stdLibKey) ]
--             $ DefManager.add (0, NodeDef.make (Type.mkPackage "Std") stdLibKey)
--             $ base_manager




full_manager :: DefManager
full_manager =  DefManager.addToParentMany [
                 (0, 1, NodeDef.make (Type.Function "Print" 
                                                    (Tuple [Type.Named "in1" $ Type.Class "Console" [], 
                                                           Type.Named "in2" $ Type.Class "String" []])
                                                    (Tuple [Type.Class "Console"[]])
                                     ) stdLibKey) 
                ]
             $ DefManager.add (0, NodeDef.make (Type.mkPackage "Std") stdLibKey)
             $ base_manager

    

    

     

