---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.CodeGenerator(
generateImportCode,
generateImportsCode,
--generateTypeCode,
--generateDefCode,
generateFunctionBody,
generateFunction
) where


import Debug.Trace

import           Data.String.Utils                 (join)
import qualified Data.Graph.Inductive            as DG
import           Control.Monad.State               (runState, get, put, State)

import qualified Luna.Type.Type                  as Type
import qualified Luna.Network.Path.Import        as Import
import           Luna.Network.Path.Import          (Import)
import qualified Luna.Network.Path.Path          as Path
import           Luna.Network.Path.Path            (Path)
import qualified Luna.Network.Graph.Graph        as Graph
import           Luna.Network.Graph.Graph          (Graph)
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Network.Def.DefManager     as DefManager
import           Luna.Network.Def.DefManager       (DefManager)
import qualified Luna.Network.Graph.Node         as Node
import           Luna.Network.Graph.Node           (Node)
import qualified Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Luna.Network.Flags              as Flags
import qualified Luna.Network.Path.Path          as Path
import           Luna.Codegen.State.FuncState      (FuncState)
import qualified Luna.Codegen.State.FuncState    as FuncState
import qualified Luna.Codegen.State.Context      as Context
import qualified Luna.Codegen.State.Mode         as Mode



generateImportCode :: Import -> String
generateImportCode i = let
    segments = Path.segments $ Import.path i
    items    = Import.items i
    import_list       = [(join "." (segments++[item]),item) | item <- items]
    simple_imports    = ["import           " ++ path ++ " as " ++ item         | (path, item) <-import_list]
    qualified_imports = ["import qualified " ++ path ++ " (" ++ item ++"(..))" | (path, item) <-import_list]
    in join "\n" (simple_imports ++ qualified_imports)


generateImportsCode :: [Import] -> String
generateImportsCode i = join "\n" $ fmap generateImportCode i


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



indent :: Int -> String
indent num = replicate (num*4) ' '


----generateImports :: NodeDef -> String
----generateImports nodeDef = foldr (++) "" imports where
----    imports = map (\a -> "import " ++ a ++ "\n") $ NodeDef.imports nodeDef




--generateFunctionBody :: NodeDef -> [String]









--generateNodeCodeLine :: Graph -> DG.LNode Node -> String
--generateNodeCodeLine graph lnode = (indent 1) ++ (generateNodeCode graph lnode) ++ "\n"

--generateFunctionReturn :: NodeDef -> String
--generateFunctionReturn nodeDef = (indent 1) ++ "in ()\n" -- TODO[PM] result list






-------------------------------------------------------------------

generateFunction :: NodeDef -> String
generateFunction def = codes where
    graph         = NodeDef.graph def
    (code1, state) = runState (generateFunctionCode def) $ FuncState.make graph
    mode = if FuncState.ctx state == Context.IO
        then Mode.ForcePure
        else Mode.ForceIO
    (code2, _)     = runState (generateFunctionCode def) $ (FuncState.make graph){FuncState.mode=mode}
    codes = code1 ++ "\n\n" ++ code2


generateFunctionBody :: NodeDef -> State FuncState String
generateFunctionBody nodeDef = do
    let
        graph      = NodeDef.graph nodeDef
        vertices   = DG.topsort $ Graph.repr graph
        nodes      = map (Graph.lnodeById graph) vertices
        nodesCodes = [] --map (generateNodeCode graph) nodes
    generateNodeCodes nodes


generateFunctionHeader :: NodeDef -> State FuncState String
generateFunctionHeader def = do
    state <- get
    let t    = NodeDef.cls def
        name = Type.name t ++ if FuncState.ctx state == Context.IO || FuncState.mode state == Mode.ForceIO
            then mpostfix
            else ""
    return $ name ++ " " ++ inputs ++ " = " 


generateFunctionCode :: NodeDef -> State FuncState String
generateFunctionCode def = do
    body   <- generateFunctionBody def
    header <- generateFunctionHeader def
    state  <- get
    let
        (ret, prefix) = if FuncState.ctx state == Context.IO || FuncState.mode state == Mode.ForceIO
            then ("return " ++ outputs, "do\n" ++ indent 1 ++ "let\n")
            else ("in "     ++ outputs, "\n"   ++ indent 1 ++ "let\n")
        code =  header ++ prefix
             ++ body
             ++ indent 1 ++ ret 
    return code


generateNodeCodes :: [DG.LNode Node] -> State FuncState String
generateNodeCodes []           = return ""
generateNodeCodes (node:nodes) = do
    --a <- begin
    --b <- test a
    prestate  <- get
    code      <- generateNodeCode  node
    poststate <- get
    childcode <- generateNodeCodes nodes
    --code = "ala" -- name node ++ "\n" ++ childcode
    -- return $ name node ++ "\n" ++ childcode
    let
        ctx = FuncState.lastctx poststate
        prefix = if ctx /= (FuncState.lastctx prestate)
            then case ctx of
                Context.IO   -> ""
                Context.Pure -> indent 1 ++ "let\n" ++ indent 1
            else case ctx of
                Context.IO   -> ""
                Context.Pure -> indent 1
    return $ prefix ++ indent 1 ++ code ++ "\n" ++ childcode


    --
    -- ++ indent 1 ++ "let\n"
    -- ++ generateFunctionBody def
    -- -- ++ indent 2 ++ join ("\n" ++ indent 2) (generateFunctionBody def) ++ "\n"
    -- ++ indent 1 ++ "in " ++ outputs


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

outvar :: Show a => a -> [Char]
outvar x = "out'" ++ show x

mpostfix :: String
mpostfix = "''M"


inputs :: String
inputs = "inputs'"


outputs :: String
outputs = "outputs'"


collectInputNum :: Graph -> Int -> [DG.Node]
collectInputNum graph nid = [num | (num,_,_) <- inedges] where
    inedges = Graph.inn graph nid


generateDefaultOutput :: Int -> State FuncState String
generateDefaultOutput nid = do
    state <- get 
    let
        inputnums = collectInputNum (FuncState.graph state) nid
        body = if null inputnums
            then "()"
            else join " " $ fmap outvar inputnums
    return body


generateNodeCode :: DG.LNode Node -> State FuncState String
generateNodeCode (nid, Node.New _ _) = do
    defout <- generateDefaultOutput nid
    return $ outvar nid ++ " = " ++ defout


generateNodeCode (nid, Node.Default (DefaultValue.DefaultString val)) = return $ outvar nid ++ " = \"" ++ val ++ "\"" 

generateNodeCode (nid, Node.Default (DefaultValue.DefaultInt val)) = return $  outvar nid ++ " = " ++ show val

generateNodeCode (nid, Node.Type name _ _ ) = 
    --"type Type'" ++ show nid ++ " = " ++ name ++ "\n" ++
    return $ outvar nid ++ " = " ++ name

generateNodeCode (nid, Node.Call name flags _ ) = do
    state <- get
    defout <- generateDefaultOutput nid    
    let isio = Flags.io flags && (FuncState.mode state /= Mode.ForcePure)
        (op, fname) = if isio 
            then ("<-", name ++ mpostfix)
            else ("=" , name)
        code = outvar nid ++ " " ++ op ++ " " ++ fname ++ " " ++ defout
    
    if isio
        then do put $ state{FuncState.ctx=Context.IO, FuncState.lastctx=Context.IO  }
        else do put $ state{                          FuncState.lastctx=Context.Pure} 
    return code
        

generateNodeCode (nid, Node.Tuple _ _) = do
    state <- get 
    let 
        inputnums = collectInputNum (FuncState.graph state) nid
        elements = join ", " $ fmap outvar inputnums
        body = if length inputnums == 1
            then "OneTuple " ++ elements
            else "(" ++ elements ++ ")"
    return $ outvar nid ++ " = " ++ body
        
            
generateNodeCode (nid, Node.Inputs _ _ ) = return $ outvar nid ++ " = " ++ inputs

generateNodeCode (nid, Node.Outputs _ _ ) = do
    defout <- generateDefaultOutput nid
    return $ outputs ++ " = " ++ defout

generateNodeCode (nid, node) = return "<not implemented>"




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




