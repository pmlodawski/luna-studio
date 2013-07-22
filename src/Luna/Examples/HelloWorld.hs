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




sample :: LibManager
sample = libman where

    ---- user generated ------------------------------------------------------------------------------------------------------------

    stdLibKey = 0
    userLibKey = 1

    defman = DefManager.addToParentMany [
                                (100, 101, myFun)]
           $ DefManager.add (100, NodeDef.make (Type.Package "user" workspaceImports) userLibKey )
           $ DefManager.addToParentMany [
                                        (8, 9, NodeDef.make (Type.makePackage "init") stdLibKey),
                                    (5, 8, NodeDef.make (Type.makePackage "String") stdLibKey),
                                    (5, 7, NodeDef.make (Type.makePackage "type") stdLibKey),
                                    (5, 6, NodeDef.make (Type.makePackage "new") stdLibKey),
                                (0, 5, NodeDef.make (Type.makePackage "types") stdLibKey),
                                        (2, 4, NodeDef.make (Type.Function "print" [Type.Class "std.io.Console" [], Type.Class "std.types.String" []] [Type.Class "std.io.Console"[]]) stdLibKey),
                                        (2, 3, NodeDef.make (Type.Function "init" [Type.Class "std.io.Console" []] [Type.Class "std.io.Console"[]]) stdLibKey),
                                    (1, 2, NodeDef.make (Type.Class "Console" []) stdLibKey),
                                (0, 1, NodeDef.make (Type.makePackage "io") stdLibKey) ]
           $ DefManager.add (0, NodeDef.make (Type.makePackage "std") stdLibKey)
           $ DefManager.empty

    libman = LibManager.register (userLibKey, Library "user" $ UniPath.fromUnixString "~/luna-projects")
           $ LibManager.register (stdLibKey,  Library "std" $ UniPath.fromUnixString "/opt/flowbox/luna/stdlib")
           $ LibManager.empty {defManager = defman }

    ---- user generated ------------------------------------------------------------------------------------------------------------

    workspaceImports = ["std.io.Console", 
                        "std.io.Console.init", 
                        "std.io.Console.print", 
                        "std.types.type", 
                        "std.types.new", 
                        "std.types.String"]

    myFun = NodeDef (Type.Function "myFun" myFunInputs Type.noOutputs) 
                     myFunGraph 
                     Flags.empty 
                     Attributes.empty 
                     userLibKey

    myFunInputs = [Type.TypeVariable "a", 
                   Type.Named "in1" $ Type.TypeVariable "b"]

    myFunGraph
     = Graph.connectMany [(0, 1, Edge 0 0 Edge.Standard),
                          (1, 2, Edge 0 0 Edge.Standard),
                          (2, 3, Edge 0 0 Edge.Standard),
                          (4, 5, Edge 0 0 Edge.Standard),
                          (5, 6, Edge 0 0 Edge.Standard),
                          (6, 8, Edge 0 0 Edge.Standard),
                          (7, 8, Edge 0 0 Edge.Standard),
                          (3, 9, Edge 0 0 Edge.Standard),
                          (8, 9, Edge 0 1 Edge.Standard)]

     $ Graph.addMany [(0, Node.DefaultNode $ DefaultValue.DefaultString "std.io.Console"),
                      (1, Node.TypeNode "std.types.type" Flags.empty Attributes.empty),
                      (2, Node.CallNode "std.types.new" Flags.empty Attributes.empty),
                      (3, Node.CallNode "std.io.Console.init" Flags.empty Attributes.empty),
                      (4, Node.DefaultNode $ DefaultValue.DefaultString "std.types.String"),
                      (5, Node.TypeNode "std.types.type" Flags.empty Attributes.empty),
                      (6, Node.CallNode "std.types.new" Flags.empty Attributes.empty),
                      (7, Node.DefaultNode $ DefaultValue.DefaultString "hello world!"),
                      (8, Node.CallNode "std.types.String.init" Flags.empty Attributes.empty),
                      (9, Node.CallNode "std.io.Console.print" Flags.empty Attributes.empty)]
     $ Graph.empty

