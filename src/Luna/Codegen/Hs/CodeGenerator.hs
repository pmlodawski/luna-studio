---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------


--generateNodeCode graph (nid, Node.TypeNode name _ _) = 

----generateCode :: Node -> DefManager -> String
----generateCode node manager = generateImports def
----                         ++ "\n\n" 
----                         ++ case node of 
----                            Node.FunctionNode _ _ -> generateFunction node
----    where
----        def = Node.def node

------- common stuff generation -----------------------------------------------------

--comment :: String
--comment = "-- "

------- function generation ---------------------------------------------------------


--generateNodeInputs :: Graph -> DG.Node -> String
--generateNodeInputs graph nid = inputs where
--    inputs = show $ DG.inn (Graph.repr graph) nid




----generateImports :: NodeDef -> String
----generateImports nodeDef = foldr (++) "" imports where
----    imports = map (\a -> "import " ++ a ++ "\n") $ NodeDef.imports nodeDef




--generateFunctionBody :: NodeDef -> [String]



--generateNodeCodeLine :: Graph -> DG.LNode Node -> String
--generateNodeCodeLine graph lnode = (indent 1) ++ (generateNodeCode graph lnode) ++ "\n"

--generateFunctionReturn :: NodeDef -> String
--generateFunctionReturn nodeDef = (indent 1) ++ "in ()\n" -- TODO[PM] result list


-------------------------------------------------------------------


--generateCode :: DG.Node -> DefManager -> String
--generateCode nid manager =  case NodeDef.cls def of 
--                            Type.Function {} -> generateFunction def
--    where
--        def = DefManager.nodeById manager nid 


---------------------------------------------

--generateNodeCode graph (nid, Node.Type name _ _) = comment ++ name ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
--generateNodeCode graph (nid, Node.Call name _ _) = comment ++ name ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
--generateNodeCode graph (nid, Node.Default (DefaultValue.DefaultInt val)) = comment ++ "<default> " ++ show val ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
--generateNodeCode graph (nid, Node.Default (DefaultValue.DefaultString val)) = comment ++ "<default> " ++ val ++ " (" ++ show nid ++ ") " ++ generateNodeInputs graph nid
--generateNodeCode graph (nid, Node.Inputs _ _) = ""
--generateNodeCode graph (nid, Node.Outputs _ _) = "<outputs>"


--generateTypeCode :: Type -> String 
--generateTypeCode t = code where
--    code = case t of
--        Type.Package name imports -> generateImportsCode imports
--        otherwise    -> "ERROR"

--generateGraphCode :: Graph -> String
--generateGraphCode g = undefined

---------------------
--generateDefCode :: DG.Node -> DefManager -> String
--generateDefCode did manager = 
--    let
--        def = DefManager.nodeById manager did 
--        cls = NodeDef.cls def
--        code = case cls of
--            Type.Package name imports         -> generateImportsCode imports
--            func@(Type.Function name inputs outputs) -> generateTypeCode func
--            _                         -> "err"
--    in code

--generateTypeCode :: Type -> String
--generateTypeCode t = case t of
--    Type.Function name inputs outputs -> name ++ " " ++ join " " signature ++ " = "
--        where signature = fmap Type.name inputs
