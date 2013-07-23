---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Samples.HelloWorld(
    workspaceImports,
    base_workspace
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



    

    
--    myFunGraph = Graph.connectMany [(0, 1, Edge 0 0 Edge.Standard),
--                          (1, 2, Edge 0 0 Edge.Standard),
--                          (2, 3, Edge 0 0 Edge.Standard),
--                          (4, 5, Edge 0 0 Edge.Standard),
--                          (5, 6, Edge 0 0 Edge.Standard),
--                          (6, 8, Edge 0 0 Edge.Standard),
--                          (7, 8, Edge 0 0 Edge.Standard),
--                          (3, 9, Edge 0 0 Edge.Standard),
--                          (8, 9, Edge 0 1 Edge.Standard)]

--               $ Graph.addMany [(0, Node.DefaultNode $ DefaultValue.DefaultString "std.io.Console"),
--                                (1, Node.TypeNode "std.types.type" Flags.empty Attributes.empty),
--                                (2, Node.CallNode "std.types.new" Flags.empty Attributes.empty),
--                                (3, Node.CallNode "std.io.Console.init" Flags.empty Attributes.empty),
--                                (4, Node.DefaultNode $ DefaultValue.DefaultString "std.types.String"),
--                                (5, Node.TypeNode "std.types.type" Flags.empty Attributes.empty),
--                                (6, Node.CallNode "std.types.new" Flags.empty Attributes.empty),
--                                (7, Node.DefaultNode $ DefaultValue.DefaultString "hello world!"),
--                                (8, Node.CallNode "std.types.String.init" Flags.empty Attributes.empty),
--                                (9, Node.CallNode "std.io.Console.print" Flags.empty Attributes.empty)]
--               $ Graph.empty

--    myFunInputs = [Type.TypeVariable "a", 
--                   Type.Named "in1" $ Type.TypeVariable "b"]

--    myFun = NodeDef (Type.Function "myFun" myFunInputs Type.noOutputs) 
--                     myFunGraph 
--                     Flags.empty 
--                     Attributes.empty 
--                     userLibKey





full_manager :: DefManager
full_manager =  DefManager.addToParentMany [
                         (8, 9, NodeDef.make (Type.mkPackage "Init") stdLibKey),
                     (5, 8, NodeDef.make (Type.mkPackage "String") stdLibKey),
                     (5, 7, NodeDef.make (Type.mkPackage "Type") stdLibKey),
                     (5, 6, NodeDef.make (Type.mkPackage "New") stdLibKey),
                 (0, 5, NodeDef.make (Type.mkPackage "Types") stdLibKey),
                         (2, 4, NodeDef.make (Type.Function "Print" [Type.Class "Console" [], Type.Class "String" []] [Type.Class "Console"[]]) stdLibKey),
                         (2, 3, NodeDef.make (Type.Function "Init" [Type.Class "Console" []] [Type.Class "Console"[]]) stdLibKey),
                     (1, 2, NodeDef.make (Type.Class "Console" []) stdLibKey),
                 (0, 1, NodeDef.make (Type.mkPackage "IO") stdLibKey) ]
             $ DefManager.add (0, NodeDef.make (Type.mkPackage "Std") stdLibKey)
             $ base_manager

    

    

     

