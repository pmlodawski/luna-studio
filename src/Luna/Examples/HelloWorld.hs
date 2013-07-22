---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Examples.HelloWorld(
    sample
) where


import qualified Data.Map                        as Map
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




sample :: LibManager
sample = libman where

    ---- user generated ------------------------------------------------------------------------------------------------------------


    defman = DefManager.addToParentMany [
                                        ]
           $ DefManager.add (100, NodeDef.empty $ Type.Package "user")
           $ DefManager.addToParentMany [
                                        (8, 9, NodeDef.empty $ Type.Package "init"),
                                    (5, 8, NodeDef.empty $ Type.Package "String"),
                                    (5, 7, NodeDef.empty $ Type.Package "type"),
                                    (5, 6, NodeDef.empty $ Type.Package "new"),
                                (0, 5, NodeDef.empty $ Type.Package "types"),
                                        (2, 4, NodeDef.empty $ Type.Function "print" [Type.Class "std.io.Console", Type.Class "std.types.String"] [Type.Class "std.io.Console"]),
                                        (2, 3, NodeDef.empty $ Type.Function "init" [Type.Class "std.io.Console"] [Type.Class "std.io.Console"]),
                                    (1, 2, NodeDef.empty $ Type.Class "Console"),
                                (0, 1, NodeDef.empty $ Type.Package "io") ]
           $ DefManager.add (0, NodeDef.empty $ Type.Package "std")
           $ DefManager.empty

    libman = LibManager.register (1, Library "user" $ UniPath.fromUnixString "~/luna-projects")
           $ LibManager.register (0, Library "std" $ UniPath.fromUnixString "/opt/flowbox/luna/stdlib")
           $ LibManager.empty {defManager = defman }

    ---- user generated ------------------------------------------------------------------------------------------------------------

    --workspace = Node.FunctionNode "myFun" $ NodeDef ["in1", "in2"] NodeDef.noPorts workspaceImports workspaceGraph workspaceKey

    --workspaceImports = ["std.io.Console", 
    --                    "std.io.Console.init", 
    --                    "std.io.Console.print", 
    --                    "std.types.type", 
    --                    "std.types.new", 
    --                    "std.types.String"]

    --workspaceGraph
    -- =  Graph.insEdges [(0, 1, Edge 0 0 Edge.Standard),
    --                    (1, 2, Edge 0 0 Edge.Standard),
    --                    (2, 3, Edge 0 0 Edge.Standard),
    --                    (4, 5, Edge 0 0 Edge.Standard),
    --                    (5, 6, Edge 0 0 Edge.Standard),
    --                    (6, 8, Edge 0 0 Edge.Standard),
    --                    (7, 8, Edge 0 0 Edge.Standard),
    --                    (3, 9, Edge 0 0 Edge.Standard),
    --                    (8, 9, Edge 0 1 Edge.Standard)]

    -- $ Graph.insNodes [(0, Node.DefaultNode $ DefaultValue.DefaultString "std.io.Console"),
    --                   (1, Node.TypeNode "std.types.type"),
    --                   (2, Node.CallNode "std.types.new"),
    --                   (3, Node.CallNode "std.io.Console.init"),
    --                   (4, Node.DefaultNode $ DefaultValue.DefaultString "std.types.String"),
    --                   (5, Node.TypeNode "std.types.type"),
    --                   (6, Node.CallNode "std.types.new"),
    --                   (7, Node.DefaultNode $ DefaultValue.DefaultString "hello world!"),
    --                   (8, Node.CallNode "std.types.String.init"),
    --                   (9, Node.CallNode "std.io.Console.print")]
    -- $ Graph.empty

