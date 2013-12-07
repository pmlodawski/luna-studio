---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Samples.HelloWorld where

--import           Flowbox.Prelude
-- TODO [PM] Compatibility after removal of Call, Type and New
-- (
--    workspacelib,
--    libman,
--    full_manager,
--    cls_vector
--) where


--import qualified Flowbox.System.UniPath                  as UniPath
--import qualified Flowbox.Luna.Network.Flags              as Flags
--import qualified Flowbox.Luna.Network.Attributes         as Attributes
--import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
--import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
--import qualified Flowbox.Luna.Network.Graph.DefaultValue as DefaultValue
--import qualified Flowbox.Luna.Network.Graph.Edge         as Edge
--import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
--import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
--import qualified Flowbox.Luna.Network.Graph.Node         as Node
--import           Flowbox.Luna.Network.Graph.Node           (Node)
--import qualified Flowbox.Luna.Network.Def.Definition     as Definition
--import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
--import qualified Flowbox.Luna.Lib.Library                as Library
--import           Flowbox.Luna.Lib.Library                  (Library(..))
--import qualified Flowbox.Luna.Lib.LibManager             as LibManager
--import           Flowbox.Luna.Lib.LibManager               (LibManager)
--import qualified Flowbox.Luna.Type.Type                  as Type
--import           Flowbox.Luna.Type.Type                    (Type(..))



----workspacedef = Definition.mkModule "Workspace"

----core = Core.insertLibDef (0, 0, workspace, workspacedef)
----     $ Core.empty


--myFunGraph :: LibManager.Gr Node Edge
--myFunGraph = Graph.insEdges [
--                                (0, 1, Edge.standard),
--                                (1, 2, Edge.standard),
--                                (2, 3, Edge.standard),
--                                (3, 4, Edge.standard),
--                                (5, 6, Edge.standard),
--                                (6, 8, Edge.standard),
--                                (7, 8, Edge.standard),
--                                (8, 9, Edge.standard),
--                                (9, 10, Edge.standard),
--                                (4, 11, Edge.standard),
--                                (10, 11, Edge.standard),
--                                (11, 12, Edge.standard)
--                               ]

--           $ Graph.insNodes [(0,  Node.mkType     "Console" ),
--                             (1,  Node.mkNew                ),
--                             (2,  Node.mkNTuple             ),
--                             (3,  Node.mkCall     "init"    ),
--                             (4,  Node.mkCall     "select0" ),
--                             (5,  Node.mkType     "String"  ),
--                             (6,  Node.mkNew                ),
--                             (7,  Node.Default (DefaultValue.DefaultString "hello world!") Attributes.empty),
--                             (8,  Node.mkNTuple             ),
--                             (9,  Node.mkCall     "init"    ),
--                             (10, Node.mkCall     "select0" ),
--                             (11, Node.mkNTuple             ),
--                             (12, Node.mkCall     "print"   ),
--                             (13, Node.mkInputs             ),
--                             (14, Node.mkOutputs            )
--                            ]
--           $ Graph.empty


--myFunInputs :: Type
--myFunInputs = Type.Tuple [Type.TypeVariable "a",
--                          Type.Named "in1" $ Type.TypeVariable "b"]


--myFun :: Definition
--myFun = Definition.empty{ Definition.cls   = (Type.Function "myFun" myFunInputs Type.noOutputs)
--                        , Definition.graph = myFunGraph
--                        }


---------------------------------------------------------------------------


--myFunGraph2 :: LibManager.Gr Node Edge
--myFunGraph2 = Graph.insEdges [(0, 1, Edge.standard),
--                              (0, 2, Edge.standard),
--                              (1, 3, Edge.standard),
--                              (2, 3, Edge.standard),
--                              (3, 4, Edge.standard),
--                              (4, 5, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0, Node.mkInputs             ),
--                             (1, Node.mkCall     "select0" ),
--                             (2, Node.mkCall     "select1" ),
--                             (3, Node.mkNTuple             ),
--                             (4, Node.mkCall     "add"     ),
--                             (5, Node.mkOutputs            )
--                            ]
--           $ Graph.empty


--myFunInputs2 :: Type
--myFunInputs2 = Type.Tuple [ Type.Named "in1" $ Type.TypeVariable "a"
--                          , Type.Named "in2" $ Type.TypeVariable "b"
--                          ]


--myFun2 :: Definition
--myFun2 = Definition.empty{ Definition.cls   = (Type.Function "myFun2" myFunInputs2 Type.noOutputs)
--                         , Definition.graph = myFunGraph2
--                         }


--myFunGraph3 :: LibManager.Gr Node Edge
--myFunGraph3 = Graph.insEdges [
--                              (0,  1,  Edge.standard),
--                              (1,  2,  Edge.standard),
--                              (2,  3,  Edge.standard),
--                              (3,  4,  Edge.standard),
--                              (5,  6,  Edge.standard),
--                              (5,  7,  Edge.standard),
--                              (6,  8,  Edge.standard),
--                              (7,  8,  Edge.standard),
--                              (8,  9,  Edge.standard),
--                              (4,  10, Edge.standard),
--                              (9,  10, Edge.standard),
--                              (10, 11, Edge.standard),
--                              (11, 13, Edge.standard),
--                              (13, 12, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkType     "Console" ),
--                             (1,  Node.mkNew                ),
--                             (2,  Node.mkNTuple             ),
--                             (3,  Node.mkCall     "init"    ),
--                             (4,  Node.mkCall     "select0" ),
--                             (5,  Node.mkInputs             ),
--                             (6,  Node.mkCall     "select0" ),
--                             (7,  Node.mkCall     "select1" ),
--                             (8,  Node.mkCall     "add"     ),
--                             (9,  Node.mkCall     "select0" ),
--                             (10, Node.mkNTuple             ),
--                             --(11, Node.Call       "print"   Flags.empty Attributes.empty),
--                             (11, Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty),
--                             (12, Node.mkOutputs            ),
--                             (13, Node.mkCall     "dummy" )
--                            ]
--           $ Graph.empty


--myFunInputs3 :: Type
--myFunInputs3 = Type.Tuple [Type.TypeVariable "a",
--                          Type.Named "in1" $ Type.TypeVariable "b"]


--myFun3 :: Definition
--myFun3 = Definition.empty{ Definition.cls   = (Type.Function "myFun3" myFunInputs3 Type.noOutputs)
--                         , Definition.graph = myFunGraph3
--                         }


--func_vec_incx_graph :: LibManager.Gr Node Edge
--func_vec_incx_graph = Graph.insEdges [
--                              (0, 1, Edge.standard),
--                              (1, 2, Edge.standard),
--                              (2, 3, Edge.standard),
--                              (3, 4, Edge.standard),
--                              (5, 6, Edge.standard),
--                              (4, 6, Edge.standard),
--                              (6, 7, Edge.standard),
--                              (7, 8, Edge.standard),
--                              (8, 9, Edge.standard),
--                              (1, 9, Edge.standard),
--                              (9, 10, Edge.standard),
--                              (10, 11, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkInputs             ),
--                             (1,  Node.mkCall     "select0" ),
--                             (2,  Node.mkNTuple             ),
--                             (3,  Node.mkCall     "x'getter"),
--                             (4,  Node.mkCall     "select0" ),
--                             (5,  Node.Default (DefaultValue.DefaultInt 1) Attributes.empty),
--                             (6,  Node.mkNTuple             ),
--                             (7,  Node.mkCall     "add"     ),
--                             (8,  Node.mkCall     "select0" ),
--                             (9,  Node.mkNTuple             ),
--                             (10, Node.mkCall     "x'setter"),
--                             (11, Node.mkOutputs            )
--                            ]
--           $ Graph.empty


--func_vec_incx_inputs :: Type
--func_vec_incx_inputs = Type.Tuple [Type.TypeVariable "a",
--                          Type.Named "in1" $ Type.TypeVariable "b"]


--func_vec_incx :: Definition
--func_vec_incx = Definition.empty{ Definition.cls   = (Type.Function "incx" func_vec_incx_inputs Type.noOutputs)
--                      , Definition.graph = func_vec_incx_graph
--                      }


--myFunGraph5 :: LibManager.Gr Node Edge
--myFunGraph5 = Graph.insEdges [
--                              (2, 3, Edge.standard),
--                              (3, 5, Edge.standard),
--                              (4, 5, Edge.standard),
--                              (5, 6, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkInputs             ),
--                             (1,  Node.mkOutputs            ),
--                             (2,  Node.mkType     "Console" ),
--                             (3,  Node.mkNew                ),
--                             (4,  Node.Default (DefaultValue.DefaultString "hello world!") Attributes.empty),
--                             (5,  Node.mkNTuple             ),
--                             (6,  Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty)
--                            ]
--           $ Graph.empty


--myFunInputs5 :: Type
--myFunInputs5 = Type.Tuple []


--myFun5 :: Definition
--myFun5 = Definition.empty{ Definition.cls   = (Type.Function "mymain" myFunInputs5 Type.noOutputs)
--                      , Definition.graph = myFunGraph5
--                      }


--func_vec_init_graph :: LibManager.Gr Node Edge
--func_vec_init_graph = Graph.insEdges [
--                              (0 , 1,  Edge.standard),
--                              (0 , 2,  Edge.standard),
--                              (0 , 3,  Edge.standard),
--                              (0 , 4,  Edge.standard),
--                              (1 , 5,  Edge.standard),
--                              (2 , 5,  Edge.standard),
--                              (5 , 6,  Edge.standard),
--                              (6 , 7,  Edge.standard),
--                              (7 , 8,  Edge.standard),
--                              (3 , 8,  Edge.standard),
--                              (8 , 9,  Edge.standard),
--                              (9 , 10, Edge.standard),
--                              (10, 11, Edge.standard),
--                              (4 , 11, Edge.standard),
--                              (11, 12, Edge.standard),
--                              (12, 13, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkInputs             ),
--                             (1,  Node.mkCall     "select0" ),
--                             (2,  Node.mkCall     "select1" ),
--                             (3,  Node.mkCall     "select2" ),
--                             (4,  Node.mkCall     "select3" ),
--                             (5,  Node.mkNTuple             ),
--                             (6,  Node.mkCall     "x'setter"),
--                             (7,  Node.mkCall     "select0" ),
--                             (8,  Node.mkNTuple             ),
--                             (9,  Node.mkCall     "y'setter"),
--                             (10, Node.mkCall     "select0" ),
--                             (11, Node.mkNTuple             ),
--                             (12, Node.mkCall     "z'setter"),
--                             (13, Node.mkOutputs            )
--                            ]
--           $ Graph.empty


--func_vec_init_inputs :: Type
--func_vec_init_inputs = Type.Tuple [Type.TypeVariable "a",
--                          Type.Named "in1" $ Type.TypeVariable "b"]


--func_vec_init :: Definition
--func_vec_init = Definition.empty{ Definition.cls   = (Type.Function "init" func_vec_init_inputs Type.noOutputs)
--                      , Definition.graph = func_vec_init_graph
--                      }



--func_main1_graph :: LibManager.Gr Node Edge
--func_main1_graph = Graph.insEdges [
--                              (2, 3, Edge.standard),
--                              (3, 5, Edge.standard),
--                              (4, 5, Edge.standard),
--                              (5, 6, Edge.standard)
--                             ]

--           $ Graph.insNodes [(0,  Node.mkInputs             ),
--                             (1,  Node.mkOutputs            ),
--                             (2,  Node.mkType     "Console" ),
--                             (3,  Node.mkNew                ),
--                             (4,  Node.Default (DefaultValue.DefaultString "hello world!") Attributes.empty),
--                             (5,  Node.mkNTuple             ),
--                             (6,  Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty)
--                            ]
--           $ Graph.empty


--func_main1_inputs :: Type
--func_main1_inputs = Type.Tuple []


--func_main1 :: Definition
--func_main1 = Definition.empty{ Definition.cls   = (Type.Function "testtypes" func_main1_inputs Type.noOutputs)
--                      , Definition.graph = func_main1_graph
--                      }


----mainFunGraph = Graph.insEdges [
----                              (2, 3, Edge.standard),
----                              (3, 5, Edge.standard),
----                              (4, 5, Edge.standard),
----                              (5, 6, Edge.standard)
----                             ]

----           $ Graph.insNodes [(0,  Node.mkInputs             ),
----                             (1,  Node.mkOutputs            ),
----                             (2,  Node.mkType     "Console" ),
----                             (3,  Node.mkNew                ),
----                             (4,  Node.Default $ DefaultValue.DefaultString "hello world!"),
----                             (5,  Node.mkNTuple             ),
----                             (6,  Node.Call       "print"   Flags.empty{Flags.io=True} Attributes.empty)
----                            ]
----           $ Graph.empty

----mainFunInputs = Type.Tuple []


----mainFun = Definition.empty{ Definition.cls   = (Type.Function "mymain" mainFunInputs Type.noOutputs)
----                      , Definition.graph = mainFunGraph
----                      }


--cls_vector :: Definition
--cls_vector = Definition.empty{ Definition.cls   = Type.Class "Vector" ["a"] [Type.Named "x" (Type.TypeVariable "a"), Type.Named "y" (Type.TypeVariable "a"), Type.Named "z" (Type.TypeVariable "a")]
--                    , Definition.graph = Graph.empty
--                    }

----iface1 = Definition.empty{  Definition.cls = Type.Interface "Ivector" ["a"] [Type.Named "x" (Type.TypeVariable "a"), Type.Named "y" (Type.TypeVariable "a"), Type.Named "z" (Type.TypeVariable "a")]
--                      --}


----full_manager :: DefManager
----full_manager =  DefManager.addToParentMany [
----                         (8, 9, Definition.make (Type.mkPackage "Init") stdLibKey),
----                     (5, 8, Definition.make (Type.mkPackage "String") stdLibKey),
----                     (5, 7, Definition.make (Type.mkPackage "Type") stdLibKey),
----                     (5, 6, Definition.make (Type.mkPackage "New") stdLibKey),
----                 (0, 5, Definition.make (Type.mkPackage "Types") stdLibKey),
----                         (2, 4, Definition.make (Type.Function "Print" [Type.Class "Console" [], Type.Class "String" []] [Type.Class "Console"[]]) stdLibKey),
----                         (2, 3, Definition.make (Type.Function "Init" [Type.Class "Console" []] [Type.Class "Console"[]]) stdLibKey),
----                     (1, 2, Definition.make (Type.Class "Console" []) stdLibKey),
----                 (0, 1, Definition.make (Type.mkPackage "IO") stdLibKey) ]
----             $ DefManager.add (0, Definition.make (Type.mkPackage "Std") stdLibKey)
----             $ base_manager



----full_manager :: DefManager
----full_manager =  DefManager.addToParentMany [(2, 5, myFun),
----                                            (2, 4, myFun2),
----                                            (2, 3, myFun3),
----                                            (1, 2, myFun3),
----                                            (100, 1, myFun2)]

--base_workspacelib :: Library
--base_workspacelib    = Library.make "Workspace" (UniPath.fromUnixString "samples/workspace/TestProject/libs")


--full_manager :: DefManager
--full_manager =  DefManager.addToParentMany [ (1, 100, func_main1),
--                                             (1, 3, func_vec_incx)
--                                           , (1, 2, func_vec_init)
--                                           , (0, 1, cls_vector)
--                                           ]
--             $ Library.defs base_workspacelib


--workspacelib :: Library
--workspacelib = base_workspacelib { defs = full_manager }

--libman :: LibManager
--libman = libman where
--    libman = LibManager.insNode (0, workspacelib)
--           $ LibManager.empty


----full_manager :: DefManager
----full_manager =  DefManager.addToParentMany [ (100, 101, testmod)
----                                           ]
----             $ base_manager


----full_manager :: DefManager
----full_manager =  DefManager.addToParentMany [
----                 (0, 1, Definition.make (Type.Function "Print"
----                                                    (Tuple [Type.Named "in1" $ Type.Class "Console" [],
----                                                           Type.Named "in2" $ Type.Class "String" []])
----                                                    (Tuple [Type.Class "Console"[]])
----                                     ) stdLibKey)
----                ]
----             $ DefManager.add (0, Definition.make (Type.mkPackage "Std") stdLibKey)
----             $ base_manager





-----------------------------------------


----stdLibKey = 0
----userLibKey = 1

----workspaceImports = [ Import (Path ["Std","IO","Console"]) ["Init", "Print"]
----                   , Import (Path ["Std","Types"])        ["Type", "New", "String"]
----                   ]


----workspace_old = Definition.empty{ Definition.cls     = Type.Module "Workspace'"
----                         , Definition.imports = workspaceImports
----                         , Definition.libID   = userLibKey
----                         }


----testmod  = Definition.empty{ Definition.cls     = Type.Module "Testmod"
----                         , Definition.imports = workspaceImports
----                         , Definition.libID   = userLibKey
----                         }


----base_manager :: DefManager
----base_manager = manager where
----    manager = DefManager.insNode (100, workspace_old)
----            $ DefManager.empty


----base_libman :: LibManager
----base_libman = libman where
----  -- TODO [PM] : insert proper root nodeDef' ids in lines below
----    libman = LibManager.insNode (userLibKey, Library "__workspace__" (UniPath.fromUnixString "lunalib/project"))
----           $ LibManager.insNode (stdLibKey,  Library "std"           (UniPath.fromUnixString "lunalib/stdlib"))
----           $ LibManager.empty




------sample :: LibManager
------sample = libman where

------    ---- user generated ------------------------------------------------------------------------------------------------------------

