---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Tools.CodeGenerator(
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
import           Luna.Node   (Node)
import qualified Luna.NodeDef as NodeDef
import           Luna.NodeDef   (NodeDef(..))
import           Luna.Library   (Library(..))


generateCode :: Node -> DefManager -> String
generateCode node manager = generateImports def
                         ++ "\n\n" 
                         ++ case node of 
                            Node.FunctionNode _ _ -> generateFunction node
    where
        def = Node.def node

--- common stuff generation -----------------------------------------------------

generateImports :: NodeDef -> String
generateImports nodeDef = foldr (++) "" imports where
    imports = map (\a -> "import " ++ a ++ "\n") $ NodeDef.imports nodeDef

indent :: Int -> String
indent num = replicate (num*4) ' '

--- function generation ---------------------------------------------------------

generateFunction :: Node -> String
generateFunction node@(Node.FunctionNode name def) = generateFunctionHeader node
                                                  ++ generateFunctionBody def
                                                  ++ generateFunctionReturn def

generateFunctionHeader :: Node -> String
generateFunctionHeader node = name ++ " " ++ arguments ++ " = \n" where
    name = Node.name node
    arguments = "" -- TODO [PM] arguments lists

generateFunctionBody :: NodeDef -> String
generateFunctionBody nodeDef = foldr (++) "" nodesCodes where
    graph = NodeDef.graph nodeDef
    rgraph = Graph.repr graph
    vertices = DG.topsort rgraph
    nodes = map (Graph.lnodeById graph) vertices
    nodesCodes = map generateNodeCodeLine nodes

generateNodeCodeLine :: DG.LNode Node -> String
generateNodeCodeLine lnode = (indent 1) ++ (generateNodeCode lnode) ++ "\n"

generateNodeCode :: DG.LNode Node -> String -- TODO [PM] finish implementation
generateNodeCode (nid, Node.TypeNode name) = name ++ " (" ++ show nid ++ ")"
generateNodeCode (nid, Node.CallNode name) = name ++ " (" ++ show nid ++ ")"
generateNodeCode (nid, Node.DefaultNode (DefaultValue.DefaultInt val)) = show val ++ " (" ++ show nid ++ ")"
generateNodeCode (nid, Node.DefaultNode (DefaultValue.DefaultString val)) = val ++ " (" ++ show nid ++ ")"

generateFunctionReturn :: NodeDef -> String
generateFunctionReturn nodeDef = (indent 1) ++ "in ()\n" -- TODO[PM] result list




