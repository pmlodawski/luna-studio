---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Samples.HelloWorld(
    workspaceImports,
    workspace,
    base_libman,
    base_manager,
    full_manager,
    myFun, myFun2, myFun3, myFun4, myFun5,
    cls1
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


workspace = NodeDef.empty{ NodeDef.cls     = Type.Module "Workspace'" 
                         , NodeDef.imports = workspaceImports
                         , NodeDef.libID   = userLibKey
                         }


testmod  = NodeDef.empty{ NodeDef.cls     = Type.Module "Testmod" 
                         , NodeDef.imports = workspaceImports
                         , NodeDef.libID   = userLibKey
                         }


base_manager :: DefManager
base_manager = manager where
    manager = DefManager.insNode (100, workspace)
            $ DefManager.empty 


base_libman :: LibManager
base_libman = libman where
  -- TODO [PM] : insert proper root nodeDef' ids in lines below
    libman = LibManager.insNode (userLibKey, Library "__workspace__" (UniPath.fromUnixString "lunalib/project") 100 ) 
           $ LibManager.insNode (stdLibKey,  Library "std"           (UniPath.fromUnixString "lunalib/stdlib") 1)
           $ LibManager.empty




--sample :: LibManager
--sample = libman where

--    ---- user generated ------------------------------------------------------------------------------------------------------------



    

    
myFunGraph = Graph.insEdges [
                                (0, 1, Edge.standard),
                                (1, 2, Edge.standard),
                                (2, 3, Edge.standard),
                                (3, 4, Edge.standard),
                                (5, 6, Edge.standard),
                                (6, 8, Edge.standard),
                                (7, 8, Edge.standard),
                                (8, 9, Edge.standard),
                                (9, 10, Edge.standard),
                                (4, 11, Edge.standard),
                                (10, 11, Edge.standard),
                                (11, 12, Edge.standard)
                               ]

           $ Graph.insNodes [(0,  Node.mkType     "Console" ),
                             (1,  Node.mkNew                ),
                             (2,  Node.mkNTuple             ),
                             (3,  Node.mkCall     "init"    ),
                             (4,  Node.mkCall     "select0" ),
                             (5,  Node.mkType     "String"  ),
                             (6,  Node.mkNew                ),
                             (7,  Node.Default $ DefaultValue.DefaultString "hello world!"),
                             (8,  Node.mkNTuple             ),
                             (9,  Node.mkCall     "init"    ),
                             (10, Node.mkCall     "select0" ),
                             (11, Node.mkNTuple             ),
                             (12, Node.mkCall     "print"   ),
                             (13, Node.mkInputs             ),
                             (14, Node.mkOutputs            )
                            ]
           $ Graph.empty

myFunInputs = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]

myFun = NodeDef.empty{ NodeDef.cls   = (Type.Function "myFun" myFunInputs Type.noOutputs)
                     , NodeDef.graph = myFunGraph
                     , NodeDef.libID = userLibKey
                     }


-----------------------------------------------------------------------



myFunGraph2 = Graph.insEdges [(0, 1, Edge.standard),
                              (0, 2, Edge.standard),
                              (1, 3, Edge.standard),
                              (2, 3, Edge.standard),
                              (3, 4, Edge.standard),
                              (4, 5, Edge.standard)
                             ] 

           $ Graph.insNodes [(0, Node.mkInputs             ),
                             (1, Node.mkCall     "select0" ),
                             (2, Node.mkCall     "select1" ),
                             (3, Node.mkNTuple             ),
                             (4, Node.mkCall     "add"     ),
                             (5, Node.mkOutputs            )
                            ]
           $ Graph.empty

myFunInputs2 = Type.Tuple [ Type.Named "in1" $ Type.TypeVariable "a"
                          , Type.Named "in2" $ Type.TypeVariable "b"
                          ]


myFun2 = NodeDef.empty{ NodeDef.cls   = (Type.Function "myFun2" myFunInputs2 Type.noOutputs)
                      , NodeDef.graph = myFunGraph2
                      , NodeDef.libID = userLibKey
                      }

myFunGraph3 = Graph.insEdges [
                              (0,  1,  Edge.standard),
                              (1,  2,  Edge.standard),
                              (2,  3,  Edge.standard),
                              (3,  4,  Edge.standard),
                              (5,  6,  Edge.standard),
                              (5,  7,  Edge.standard),
                              (6,  8,  Edge.standard),
                              (7,  8,  Edge.standard),
                              (8,  9,  Edge.standard),
                              (4,  10, Edge.standard),
                              (9,  10, Edge.standard),
                              (10, 11, Edge.standard),
                              (11, 13, Edge.standard),
                              (13, 12, Edge.standard)
                             ]

           $ Graph.insNodes [(0,  Node.mkType     "Console" ),
                             (1,  Node.mkNew                ),
                             (2,  Node.mkNTuple             ),
                             (3,  Node.mkCall     "init"    ),
                             (4,  Node.mkCall     "select0" ),
                             (5,  Node.mkInputs             ),
                             (6,  Node.mkCall     "select0" ),
                             (7,  Node.mkCall     "select1" ),
                             (8,  Node.mkCall     "add"     ),
                             (9,  Node.mkCall     "select0" ),
                             (10, Node.mkNTuple             ),
                             --(11, Node.Call       "print"   Flags.empty Attributes.empty),
                             (11, Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty),
                             (12, Node.mkOutputs            ),
                             (13, Node.mkCall     "dummy" )
                            ]
           $ Graph.empty

myFunInputs3 = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]


myFun3 = NodeDef.empty{ NodeDef.cls   = (Type.Function "myFun3" myFunInputs3 Type.noOutputs)
                      , NodeDef.graph = myFunGraph3
                      , NodeDef.libID = userLibKey
                      }




myFunGraph4 = Graph.insEdges [
                              (0, 1, Edge.standard),
                              (1, 2, Edge.standard),
                              (2, 3, Edge.standard),
                              (3, 4, Edge.standard),
                              (5, 6, Edge.standard),
                              (4, 6, Edge.standard),
                              (6, 7, Edge.standard),
                              (7, 8, Edge.standard),
                              (8, 9, Edge.standard),
                              (1, 9, Edge.standard),
                              (9, 10, Edge.standard),
                              (10, 11, Edge.standard)
                             ]

           $ Graph.insNodes [(0,  Node.mkInputs             ),
                             (1,  Node.mkCall     "select0" ),
                             (2,  Node.mkNTuple             ),
                             (3,  Node.mkCall     "x'getter"),
                             (4,  Node.mkCall     "select0" ),
                             (5,  Node.Default $ DefaultValue.DefaultInt 1),
                             (6,  Node.mkNTuple             ),
                             (7,  Node.mkCall     "add"     ),
                             (8,  Node.mkCall     "select0" ),
                             (9,  Node.mkNTuple             ),
                             (10, Node.mkCall     "x'setter"),
                             (11, Node.mkOutputs            )
                            ]
           $ Graph.empty

myFunInputs4 = Type.Tuple [Type.TypeVariable "a", 
                          Type.Named "in1" $ Type.TypeVariable "b"]


myFun4 = NodeDef.empty{ NodeDef.cls   = (Type.Function "incx" myFunInputs4 Type.noOutputs)
                      , NodeDef.graph = myFunGraph4
                      , NodeDef.libID = userLibKey
                      }






myFunGraph5 = Graph.insEdges [
                              (2, 3, Edge.standard),
                              (3, 5, Edge.standard),
                              (4, 5, Edge.standard),
                              (5, 6, Edge.standard)
                             ]

           $ Graph.insNodes [(0,  Node.mkInputs             ),
                             (1,  Node.mkOutputs            ),
                             (2,  Node.mkType     "Console" ),
                             (3,  Node.mkNew                ),
                             (4,  Node.Default $ DefaultValue.DefaultString "hello world!"),
                             (5,  Node.mkNTuple             ),
                             (6,  Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty)
                            ]
           $ Graph.empty

myFunInputs5 = Type.Tuple []


myFun5 = NodeDef.empty{ NodeDef.cls   = (Type.Function "mymain" myFunInputs5 Type.noOutputs)
                      , NodeDef.graph = myFunGraph5
                      , NodeDef.libID = userLibKey
                      }




cls1 = NodeDef.empty{ NodeDef.cls   = Type.Class "Vector" ["a"] [Type.Named "x" (Type.TypeVariable "a"), Type.Named "y" (Type.TypeVariable "a"), Type.Named "z" (Type.TypeVariable "a")]
                    , NodeDef.graph = Graph.empty
                    , NodeDef.libID = userLibKey
                    }

--iface1 = NodeDef.empty{  NodeDef.cls = Type.Interface "Ivector" ["a"] [Type.Named "x" (Type.TypeVariable "a"), Type.Named "y" (Type.TypeVariable "a"), Type.Named "z" (Type.TypeVariable "a")]
                      --}


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
full_manager =  DefManager.addToParentMany [(2, 5, myFun),
                                            (2, 4, myFun2),
                                            (2, 3, myFun3),
                                            (1, 2, myFun3),
                                            (100, 1, myFun2)]
--full_manager =  DefManager.addToParentMany [ --(1, 2, myFun2),
--                                           (10, 1, myFun5),
--                                           (100, 10, cls1)
--                                           ]
             $ base_manager

--full_manager :: DefManager
--full_manager =  DefManager.addToParentMany [ (100, 101, testmod)
--                                           ]
--             $ base_manager


--full_manager :: DefManager
--full_manager =  DefManager.addToParentMany [
--                 (0, 1, NodeDef.make (Type.Function "Print" 
--                                                    (Tuple [Type.Named "in1" $ Type.Class "Console" [], 
--                                                           Type.Named "in2" $ Type.Class "String" []])
--                                                    (Tuple [Type.Class "Console"[]])
--                                     ) stdLibKey) 
--                ]
--             $ DefManager.add (0, NodeDef.make (Type.mkPackage "Std") stdLibKey)
--             $ base_manager

    

    

     

