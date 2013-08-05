---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.FuncGenerator(
generateFunction
) where


import qualified Luna.Type.Type                  as Type
import qualified Luna.Codegen.Hs.Import          as Import
import qualified Luna.Network.Graph.Graph        as Graph
import           Luna.Network.Graph.Graph          (Graph)
import qualified Luna.Network.Def.NodeDef        as NodeDef
import           Luna.Network.Def.NodeDef          (NodeDef)
import qualified Luna.Network.Graph.Node         as Node
import           Luna.Network.Graph.Node           (Node)
import qualified Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Luna.Network.Flags              as Flags
import qualified Luna.Codegen.Hs.Path            as Path


import qualified Luna.Codegen.Hs.AST.Function    as Function
import           Luna.Codegen.Hs.AST.Function      (Function)
import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import qualified Luna.Codegen.Hs.AST.Module      as Module
import           Luna.Codegen.Hs.AST.Module        (Module)

import           Luna.Data.List


generateFunction :: NodeDef -> Module -> (Function, Module)
generateFunction def m = (func, nmod) where
    graph      = NodeDef.graph def
    vertices   = Graph.topsort graph
    nodes      = Graph.labVtxs graph vertices
    fcls       = NodeDef.cls def
    fname      = Type.name fcls
    --finputs    = Type.inputs fcls
    basefunc   = Function.basic { Function.name = fname
                                --, Function.signature = [Expr.At Path.inputs (Expr.Tuple (replicate 3 Expr.Any))]
                                }
    (func, nmod) = foldri (generateNodeExpr graph) nodes (basefunc, m)


generateNodeExpr :: Graph -> Graph.LVertex Node -> (Function, Module) -> (Function, Module)
generateNodeExpr graph lnode (func, m) = (nfunc, nmod) where
    block  = Function.body func
    nblock = Expr.addExpr expr block
    nfunc  = Function.setBody nblock
           $ case ctx of
                 Expr.IO -> Function.setCtx ctx func
                 _       -> func
          
    (nid, node) = lnode
    expr  = Expr.Assignment src value ctx 
    src   = case node of
        Node.Outputs {}         -> Expr.Var Path.outputs
        _                       -> Expr.VarRef nid

    (value, ctx) = case node of
        Node.New _ _            -> (Expr.VarRef cvtx, Expr.Pure) where
                                       cvtx:_ = Graph.innvtx graph nid
                                       -- TODO[wd] exception when too many inputs

        Node.Type name _ _      -> (Expr.Var name, Expr.Pure)

        Node.Tuple _ _          -> (Expr.Tuple args, Expr.Pure) where
                                       vtxs = Graph.innvtx graph nid
                                       args = map Expr.VarRef vtxs

        Node.NTuple _ _         -> (Expr.NTuple args, Expr.Pure) where
                                       vtxs = Graph.innvtx graph nid
                                       args = map Expr.VarRef vtxs

        Node.Call name flags _  -> (Expr.Call name args cctx, cctx) where
                                       vtxs = Graph.innvtx graph nid
                                       args = map Expr.VarRef vtxs
                                       cctx = if Flags.io flags
                                           then Expr.IO
                                           else Expr.Pure

        Node.Inputs  _ _        -> (Expr.Var Path.inputs, Expr.Pure)
        Node.Outputs _ _        -> (Expr.VarRef cvtx, Expr.Pure) where
                                        cvtx:_ = Graph.innvtx graph nid
                                        -- TODO[wd] exception when too many inputs

        Node.Default d          -> (Expr.Default val, Expr.Pure) where 
                                       val = case d of
                                           DefaultValue.DefaultString v -> show v
                                           DefaultValue.DefaultInt    v -> show v ++ " :: Int"

    nmod = case node of 
        Node.Call name _ _  -> Module.addImport (Import.common name)
                             $ m
        _                   -> m




--indent :: Int -> String
--indent num = replicate (num*4) ' '

--mpostfix :: String
--mpostfix = "''M"

--outvar :: Show a => a -> [Char]
--outvar x = "out'" ++ show x

--generateImportCode :: Import -> String
--generateImportCode i = let
--    segments = Path.segments $ Import.path i
--    items    = Import.items i
--    import_list       = [(join "." (segments++[item]),item) | item <- items]
--    simple_imports    = ["import           " ++ path ++ " as " ++ item         | (path, item) <-import_list]
--    qualified_imports = ["import qualified " ++ path ++ " (" ++ item ++"(..))" | (path, item) <-import_list]
--    in join "\n" (simple_imports ++ qualified_imports)


--generateImportsCode :: [Import] -> String
--generateImportsCode i = join "\n" $ fmap generateImportCode i

--generateNodeCode (nid, Node.Call name flags _ ) = do
--    state <- get
--    defout <- generateDefaultOutput nid    
--    let isio = Flags.io flags && (FuncState.mode state /= Mode.ForcePure)
--        (op, fname) = if isio 
--            then ("<-", name ++ mpostfix)
--            else ("=" , name)
--        code = outvar nid ++ " " ++ op ++ " " ++ fname ++ " " ++ defout
    
--    if isio
--        then do put $ state{FuncState.ctx=Context.IO, FuncState.lastctx=Context.IO  }
--        else do put $ state{                          FuncState.lastctx=Context.Pure} 
--    return code


--generateFunctionBody :: State FuncState String
--generateFunctionBody = do
--    state <- get
--    let
--        graph      = FuncState.graph state
--        vertices   = Graph.topsort graph
--        nodes      = Graph.labVtxs graph vertices
--    generateNodeCodes nodes


--generateFunctionHeader :: State FuncState String
--generateFunctionHeader = do
--    state <- get
--    let t    = NodeDef.cls $ FuncState.def state
--        name = Type.name t ++ if FuncState.ctx state == Context.IO || FuncState.mode state == Mode.ForceIO
--            then mpostfix
--            else ""
--    return $ name ++ " " ++ inputs ++ " = " 


--generateFunctionCode :: State FuncState String
--generateFunctionCode = do
--    body   <- generateFunctionBody
--    header <- generateFunctionHeader
--    state  <- get
--    let
--        (ret, prefix) = if FuncState.ctx state == Context.IO || FuncState.mode state == Mode.ForceIO
--            then ("return " ++ outputs, "do\n" ++ indent 1 ++ "let\n")
--            else ("in "     ++ outputs, "\n"   ++ indent 1 ++ "let\n")
--        code =  header ++ prefix
--             ++ body
--             ++ indent 1 ++ ret 
--    return code


--generateNodeCodes :: [DG.LNode Node] -> State FuncState String
--generateNodeCodes []           = return ""
--generateNodeCodes (node:nodes) = do
--    prestate  <- get
--    code      <- generateNodeCode  node
--    poststate <- get
--    childcode <- generateNodeCodes nodes

--    let
--        ctx = FuncState.lastctx poststate
--        prefix = if ctx /= (FuncState.lastctx prestate)
--            then case ctx of
--                Context.IO   -> ""
--                Context.Pure -> indent 1 ++ "let\n" ++ indent 1
--            else case ctx of
--                Context.IO   -> ""
--                Context.Pure -> indent 1
--    return $ prefix ++ indent 1 ++ code ++ "\n" ++ childcode


--collectInputNum :: Graph -> Int -> [DG.Node]
--collectInputNum graph nid = [num | (num,_,_) <- inedges] where
--    inedges = Graph.inn graph nid


--generateDefaultOutput :: Int -> State FuncState String
--generateDefaultOutput nid = do
--    state <- get 
--    let
--        inputnums = collectInputNum (FuncState.graph state) nid
--        body = if null inputnums
--            then "()"
--            else join " " $ fmap outvar inputnums
--    return body


--generateNodeCode :: DG.LNode Node -> State FuncState String
--generateNodeCode (nid, Node.New _ _) = do
--    defout <- generateDefaultOutput nid
--    return $ outvar nid ++ " = " ++ defout

--generateNodeCode (nid, Node.Default (DefaultValue.DefaultString val)) = return $ outvar nid ++ " = \"" ++ val ++ "\"" 

--generateNodeCode (nid, Node.Default (DefaultValue.DefaultInt val)) = return $  outvar nid ++ " = " ++ show val

--generateNodeCode (nid, Node.Type name _ _ ) = 
--    --"type Type'" ++ show nid ++ " = " ++ name ++ "\n" ++
--    return $ outvar nid ++ " = " ++ name

--generateNodeCode (nid, Node.Call name flags _ ) = do
--    state <- get
--    defout <- generateDefaultOutput nid    
--    let isio = Flags.io flags && (FuncState.mode state /= Mode.ForcePure)
--        (op, fname) = if isio 
--            then ("<-", name ++ mpostfix)
--            else ("=" , name)
--        code = outvar nid ++ " " ++ op ++ " " ++ fname ++ " " ++ defout
    
--    if isio
--        then do put $ state{FuncState.ctx=Context.IO, FuncState.lastctx=Context.IO  }
--        else do put $ state{                          FuncState.lastctx=Context.Pure} 
--    return code
        
--generateNodeCode (nid, Node.Tuple _ _) = do
--    state <- get 
--    let 
--        inputnums = collectInputNum (FuncState.graph state) nid
--        elements = join ", " $ fmap outvar inputnums
--        body = if length inputnums == 1
--            then "OneTuple " ++ elements
--            else "(" ++ elements ++ ")"
--    return $ outvar nid ++ " = " ++ body
                  
--generateNodeCode (nid, Node.Inputs _ _ ) = return $ outvar nid ++ " = " ++ inputs

--generateNodeCode (nid, Node.Outputs _ _ ) = do
--    defout <- generateDefaultOutput nid
--    return $ outputs ++ " = " ++ defout

--generateNodeCode (nid, node) = return "<not implemented>"

