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
    myFun, myFun2, myFun3
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



    

    
myFunGraph = Graph.connectMany [
                                (0, 1, Edge.Standard),
                                (1, 2, Edge.Standard),
                                (2, 3, Edge.Standard),
                                (3, 4, Edge.Standard),
                                (5, 6, Edge.Standard),
                                (6, 8, Edge.Standard),
                                (7, 8, Edge.Standard),
                                (8, 9, Edge.Standard),
                                (9, 10, Edge.Standard),
                                (4, 11, Edge.Standard),
                                (10, 11, Edge.Standard),
                                (11, 12, Edge.Standard)
                               ]

           $ Graph.addMany [(0,  Node.mkType     "Console" ),
                            (1,  Node.mkNew                ),
                            (2,  Node.mkTuple              ),
                            (3,  Node.mkCall     "init"    ),
                            (4,  Node.mkCall     "select0" ),
                            (5,  Node.mkType     "String"  ),
                            (6,  Node.mkNew                ),
                            (7,  Node.Default $ DefaultValue.DefaultString "hello world!"),
                            (8,  Node.mkTuple              ),
                            (9,  Node.mkCall     "init"    ),
                            (10, Node.mkCall     "select0" ),
                            (11, Node.mkTuple              ),
                            (12, Node.mkCall     "print"   ),
                            (13, Node.mkInputs             ),
                            (14, Node.mkOutputs            )
                           ]
           $ Graph.empty

myFunInputs = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]

myFun = NodeDef (Type.Function "myFun" myFunInputs Type.noOutputs) 
                 myFunGraph 
                 Flags.empty 
                 Attributes.empty 
                 userLibKey



myFunGraph2 = Graph.connectMany [(0, 1, Edge.Standard),
                                 (0, 2, Edge.Standard),
                                 (1, 3, Edge.Standard),
                                 (2, 3, Edge.Standard),
                                 (3, 4, Edge.Standard),
                                 (4, 5, Edge.Standard)
                                ]

           $ Graph.addMany [(0, Node.mkInputs             ),
                            (1, Node.mkCall     "select0" ),
                            (2, Node.mkCall     "select1" ),
                            (3, Node.mkTuple              ),
                            (4, Node.mkCall     "add"     ),
                            (5, Node.mkOutputs            )
                           ]
           $ Graph.empty

myFunInputs2 = Type.Tuple [Type.TypeVariable "a", 
                           Type.Named "in1" $ Type.TypeVariable "b"]

myFun2 = NodeDef (Type.Function "myFun2" myFunInputs2 Type.noOutputs) 
                 myFunGraph2 
                 Flags.empty 
                 Attributes.empty 
                 userLibKey


myFunGraph3 = Graph.connectMany [
                                (0, 1, Edge.Standard),
                                (1, 2, Edge.Standard),
                                (2, 3, Edge.Standard),
                                (3, 4, Edge.Standard),
                                (5, 6, Edge.Standard),
                                (5, 7, Edge.Standard),
                                (6, 8, Edge.Standard),
                                (7, 8, Edge.Standard),
                                (8, 9, Edge.Standard),
                                (4, 10, Edge.Standard),
                                (9, 10, Edge.Standard),
                                (10, 11, Edge.Standard)
                               ]

           $ Graph.addMany [(0,  Node.mkType     "Console" ),
                            (1,  Node.mkNew                ),
                            (2,  Node.mkTuple              ),
                            (3,  Node.mkCall     "init"    ),
                            (4,  Node.mkCall     "select0" ),
                            (5,  Node.mkInputs             ),
                            (6,  Node.mkCall     "select0" ),
                            (7,  Node.mkCall     "select1" ),
                            (8,  Node.mkCall     "add"     ),
                            (9,  Node.mkCall     "select0" ),
                            (10, Node.mkTuple              ),
                            (11, Node.mkCall     "print"   )
                           ]
           $ Graph.empty

myFunInputs3 = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]

myFun3 = NodeDef (Type.Function "myFun3" myFunInputs3 Type.noOutputs) 
                 myFunGraph3 
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

    

    

     

