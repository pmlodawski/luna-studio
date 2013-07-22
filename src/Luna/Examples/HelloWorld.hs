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
           stdlib = Library $ UniPath.fromUnixString "/opt/flowbox/luna/stdlib"
           libman = LibManager.register (0, stdlib)
                  $ LibManager.empty

           n_std   = NodeDef.empty $ Type.Package "std"
           manager = DefManager.add (0, n_std)
                   $ DefManager.empty

    --stdlibKey    = 0
    --workspaceKey = 1

    --librariesMap = Map.fromList [(stdlibKey,    Library $ UniPath.fromUnixString "stdlib"),
    --                             (workspaceKey, Library $ UniPath.fromUnixString "workspace")]
    
    --manager = DefManager librariesMap root_graph 

    --root_graph = Graph.insNodes [(0, Node.PackageNode "std" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_graph      stdlibKey),
    --                             (1, workspace)]
    --           $ Graph.empty

    ---- std lib -------------------------------------------------------------------------------------------------------------------

    --std_graph
    -- = Graph.insNodes [(0, Node.PackageNode "types" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_types_graph stdlibKey),
    --                   (1, Node.PackageNode "io"    $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_io_graph    stdlibKey)]
    -- $ Graph.empty

    --std_types_graph 
    -- = Graph.insNodes [(0, Node.ClassNode    "String" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_types_String_graph stdlibKey),
    --                   (1, Node.FunctionNode "new"    $ NodeDef ["type"]        ["instance"]    NodeDef.noImports Graph.empty            stdlibKey),
    --                   (2, Node.FunctionNode "type"   $ NodeDef ["name"]        ["type"]        NodeDef.noImports Graph.empty            stdlibKey)]
    -- $ Graph.empty

    --std_types_String_graph 
    -- = Graph.insNode (0, Node.FunctionNode "init" $ NodeDef ["self", "value"] ["instance"] NodeDef.noImports Graph.empty stdlibKey)
    -- $ Graph.empty

    --std_io_graph
    -- = Graph.insNode (0, Node.ClassNode "Console" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_io_Console_graph stdlibKey)
    -- $ Graph.empty

    --std_io_Console_graph 
    -- = Graph.insNodes [(0, Node.FunctionNode "print" $ NodeDef ["self", "value"] ["console"] NodeDef.noImports Graph.empty stdlibKey),
    --                   (1, Node.FunctionNode "init"  $ NodeDef ["self"]         ["instance"] NodeDef.noImports Graph.empty stdlibKey)]
    -- $ Graph.empty


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

