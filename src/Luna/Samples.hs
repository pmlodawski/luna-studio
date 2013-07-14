---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Samples(
sample_helloWorld
) where

import qualified Data.Map            as Map
import           Luna.DefManager       (DefManager(..))
import qualified Luna.DefaultValue   as DefaultValue
import qualified Luna.Edge           as Edge
import           Luna.Edge             (Edge(..))
import qualified Luna.Graph          as Graph
import           Luna.Graph            (Graph)
import qualified Luna.Node           as Node
import qualified Luna.NodeDef        as NodeDef
import           Luna.NodeDef          (NodeDef(..))
import           Luna.Library          (Library(..))
import qualified Luna.System.UniPath as UniPath


sample_helloWorld :: (Graph, DefManager)
sample_helloWorld = (workspaceGraph, manager) where
    
    stdlibKey    = 0
    workspaceKey = 1

    librariesMap = Map.fromList [(stdlibKey,    Library $ UniPath.fromUnixString "stdlib"),
                                 (workspaceKey, Library $ UniPath.fromUnixString "workspace")]
    
    manager = DefManager librariesMap root_graph 

    root_graph = Graph.insNodes [(0, Node.PackageNode "std" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_graph      stdlibKey),
                                 (1, Node.PackageNode "my"  $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports workspaceGraph workspaceKey)]
               $ Graph.empty

    std_graph
     = Graph.insNodes [(0, Node.PackageNode "types" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_types_graph stdlibKey),
                       (1, Node.PackageNode "io"    $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_io_graph    stdlibKey)]
     $ Graph.empty

    std_types_graph 
     = Graph.insNodes [(0, Node.ClassNode    "String" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_types_String_graph stdlibKey),
                       (1, Node.FunctionNode "new"    $ NodeDef ["type"]        ["instance"]    NodeDef.noImports Graph.empty            stdlibKey),
                       (2, Node.FunctionNode "type"   $ NodeDef ["name"]        ["type"]        NodeDef.noImports Graph.empty            stdlibKey)]
     $ Graph.empty

    std_types_String_graph 
     = Graph.insNode (0, Node.FunctionNode "init" $ NodeDef ["self", "value"] ["instance"] NodeDef.noImports Graph.empty stdlibKey)
     $ Graph.empty

    std_io_graph
     = Graph.insNode (0, Node.ClassNode "Console" $ NodeDef NodeDef.noPorts NodeDef.noPorts NodeDef.noImports std_io_Console_graph stdlibKey)
     $ Graph.empty

    std_io_Console_graph 
     = Graph.insNodes [(0, Node.FunctionNode "print" $ NodeDef ["self", "value"] ["console"] NodeDef.noImports Graph.empty stdlibKey),
                       (1, Node.FunctionNode "init"  $ NodeDef ["self"]         ["instance"] NodeDef.noImports Graph.empty stdlibKey)]
     $ Graph.empty

    workspaceGraph
     =  Graph.insEdges [(0, 1, Edge "value" "name" Edge.Standard),
                        (1, 2, Edge "type" "type" Edge.Standard),
                        (2, 3, Edge "instance" "self" Edge.Standard),
                        (4, 5, Edge "value" "name" Edge.Standard),
                        (5, 6, Edge "type" "type" Edge.Standard),
                        (6, 8, Edge "instance" "self" Edge.Standard),
                        (7, 8, Edge "value" "value" Edge.Standard),
                        (3, 9, Edge "instance" "self" Edge.Standard),
                        (8, 9, Edge "instance" "value" Edge.Standard)]

     $ Graph.insNodes [(0, Node.DefaultNode $ DefaultValue.DefaultString "std.io.Console"),
                       (1, Node.TypeNode "std.types.type"),
                       (2, Node.CallNode "std.types.new"),
                       (3, Node.CallNode "std.io.Console.init"),
                       (4, Node.DefaultNode $ DefaultValue.DefaultString "std.types.String"),
                       (5, Node.TypeNode "std.types.type"),
                       (6, Node.CallNode "std.types.new"),
                       (7, Node.DefaultNode $ DefaultValue.DefaultString "hello world!"),
                       (8, Node.CallNode "std.types.String.init"),
                       (9, Node.CallNode "std.io.Console.print")]
     $ Graph.empty

