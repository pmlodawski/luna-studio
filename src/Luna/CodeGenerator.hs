---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.CodeGenerator(
generateCode
) where

import qualified Data.Graph.Inductive as DG
import qualified Data.Map   as Map

import           Luna.DefManager   (DefManager(..))
import qualified Luna.DefaultValue as DefaultValue
import qualified Luna.Edge as Edge
import           Luna.Edge   (Edge(..))
import qualified Luna.Graph as Graph
import           Luna.Graph   (Graph(..))
import qualified Luna.Node as Node
import qualified Luna.NodeDef as NodeDef
import           Luna.NodeDef   (NodeDef(..))
import           Luna.Library   (Library(..))
import qualified System.UniPath as UniPath


generateImports :: NodeDef -> String
generateImports nodeDef = foldr (++) "" imports where
    imports = map (++"\n") $ map ("import "++ ) $ NodeDef.imports nodeDef

generateFunctionHeader :: NodeDef -> String
generateFunctionHeader nodeDef = name ++ " " ++ arguments ++ " = \n" where
    name = "dummy"
    arguments = ""

generateFunctionBody :: NodeDef -> String
generateFunctionBody nodeDef = show nodes where
    graph = NodeDef.graph nodeDef
    rgraph = Graph.repr graph
    vertices = DG.topsort rgraph
    nodes = map (Graph.nodeById graph) vertices


generateCode :: NodeDef -> DefManager -> String
generateCode nodeDef manager = (generateImports nodeDef)
                            ++ "\n\n" 
                            ++ (generateFunctionHeader nodeDef)
                            ++ (generateFunctionBody nodeDef)