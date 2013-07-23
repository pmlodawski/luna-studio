---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Tools.CodeGenerator(
generateImportCode,
generateImportsCode,
generateTypeCode,
generateDefCode,
generateFunctionBody
) where


import Debug.Trace

import           Data.String.Utils                 (join)
import qualified Data.Graph.Inductive            as DG

import qualified Luna.Type.Type                  as Type
import           Luna.Type.Type                    (Type)
import qualified Luna.Network.Path.Import        as Import
import           Luna.Network.Path.Import          (Import(..))
import qualified Luna.Network.Path.Path          as Path
import qualified Luna.Network.Graph.Graph        as Graph
import           Luna.Network.Graph.Graph          (Graph(..))
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef(..))
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager)
import qualified Luna.Network.Graph.Node         as Node
import           Luna.Network.Graph.Node           (Node)
import qualified Luna.Network.Graph.DefaultValue as DefaultValue


generateImportCode :: Import -> String
generateImportCode i = let
    segments = Path.segments $ Import.path i
    items    = Import.items i
    import_list       = [(join "." (segments++[i]),i) | i <- items]
    simple_imports    = ["import           " ++ path ++ " as " ++ item         | (path, item) <-import_list]
    qualified_imports = ["import qualified " ++ path ++ " (" ++ item ++"(..))" | (path, item) <-import_list]
    in join "\n" (simple_imports++qualified_imports)


generateImportsCode :: [Import] -> String
generateImportsCode i = join "\n" $ fmap generateImportCode i


--generateTypeCode :: Type -> String 
--generateTypeCode t = code where
--    code = case t of
--        Type.Package name imports -> generateImportsCode imports
--        otherwise    -> "ERROR"

generateGraphCode :: Graph -> String
generateGraphCode g = undefined

generateDefCode :: DG.Node -> DefManager -> String
generateDefCode did manager = 
    let
        def = DefManager.nodeById manager did 
        cls = NodeDef.cls def
        code = case cls of
            Type.Package name imports         -> generateImportsCode imports
            func@(Type.Function name inputs outputs) -> generateTypeCode func
            _                         -> "err"
    in code

generateTypeCode :: Type -> String
generateTypeCode t = case t of
    Type.Function name inputs outputs -> name ++ " " ++ join " " signature ++ " = "
        where signature = fmap Type.name inputs



indent :: Int -> String
indent num = replicate (num*4) ' '


----generateImports :: NodeDef -> String
----generateImports nodeDef = foldr (++) "" imports where
----    imports = map (\a -> "import " ++ a ++ "\n") $ NodeDef.imports nodeDef

--generateFunctionHeader :: NodeDef -> String
--generateFunctionHeader def = name ++ " " ++ arguments ++ " = \n" where
--    t    = NodeDef.cls def
--    name = Type.name t
--    arguments = join " " (Type.inputs t)

generateFunctionBody :: NodeDef -> String
generateFunctionBody nodeDef = foldr (++) "" nodesCodes where
    graph      = NodeDef.graph nodeDef
    vertices   = DG.topsort $ Graph.repr graph
    nodes      = map (Graph.lnodeById graph) vertices
    nodesCodes = map (generateNodeCodeLine graph) nodes

generateNodeCodeLine :: Graph -> DG.LNode Node -> String
generateNodeCodeLine graph lnode = (indent 1) ++ (generateNodeCode graph lnode) ++ "\n"

--generateFunctionReturn :: NodeDef -> String
--generateFunctionReturn nodeDef = (indent 1) ++ "in ()\n" -- TODO[PM] result list


--generateFunction :: NodeDef -> String
--generateFunction def = generateFunctionHeader def
--                    -- ++ generateFunctionBody   def
--                    -- ++ generateFunctionReturn def


--generateCode :: DG.Node -> DefManager -> String
--generateCode nid manager =  case NodeDef.cls def of 
--                            Type.Function {} -> generateFunction def
--    where
--        def = DefManager.nodeById manager nid 


---------------------------------------------

--generateNodeCode graph (nid, Node.Type name _ _) = comment ++ name ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
generateNodeCode graph (nid, Node.Call name _ _) = comment ++ name ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
generateNodeCode graph (nid, Node.Default (DefaultValue.DefaultInt val)) = comment ++ "<default> " ++ show val ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
--generateNodeCode graph (nid, Node.Default (DefaultValue.DefaultString val)) = comment ++ "<default> " ++ val ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid

generateNodeCode graph (nid, Node.Default (DefaultValue.DefaultString val)) = "out_" ++ show nid ++ " = \"" ++ val ++ "\"" 
generateNodeCode graph (nid, Node.Type name _ _)                            = "type Type_" ++ show nid ++ " = " ++ name 


--generateNodeCode graph (nid, Node.TypeNode name _ _) = 

----generateCode :: Node -> DefManager -> String
----generateCode node manager = generateImports def
----                         ++ "\n\n" 
----                         ++ case node of 
----                            Node.FunctionNode _ _ -> generateFunction node
----    where
----        def = Node.def node

------- common stuff generation -----------------------------------------------------

comment :: String
comment = "-- "

------- function generation ---------------------------------------------------------








generateNodeInputs :: Graph -> DG.Node -> String
generateNodeInputs graph nid = inputs where
    inputs = show $ DG.inn (Graph.repr graph) nid






