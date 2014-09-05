---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Luna.Pass.Transform.Graph.Builder.Builder where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe

import           Flowbox.Prelude                         hiding (error, mapM, mapM_)
import qualified Flowbox.Prelude                         as Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                         as AST
import           Luna.AST.Expr                           (Expr)
import qualified Luna.AST.Expr                           as Expr
import qualified Luna.AST.Lit                            as Lit
import           Luna.AST.Pat                            (Pat)
import qualified Luna.AST.Pat                            as Pat
import qualified Luna.AST.Type                           as Type
import           Luna.Data.AliasInfo                     (AliasInfo)
import           Luna.Graph.Graph                        (Graph)
import qualified Luna.Graph.Node                         as Node
import qualified Luna.Graph.Node.OutputName              as OutputName
import           Luna.Graph.Port                         (InPort)
import qualified Luna.Graph.Port                         as Port
import           Luna.Graph.PropertyMap                  (PropertyMap)
import qualified Luna.Pass.Pass                          as Pass
import           Luna.Pass.Transform.Graph.Builder.State (GBPass)
import qualified Luna.Pass.Transform.Graph.Builder.State as State



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"




run :: AliasInfo -> PropertyMap -> Expr -> Pass.Result (Graph, PropertyMap)
run aliasInfo pm expr = Pass.run_ (Pass.Info "GraphBuilder")
                                  (State.make aliasInfo pm inputsID)
                                  (expr2graph expr)
    where inputsID = - expr ^. Expr.id


expr2graph :: Expr -> GBPass (Graph, PropertyMap)
expr2graph (Expr.Function i _ _ inputs output body) = do
    (inputsID, outputID) <- prepareInputsOutputs i (output ^. Type.id)
    parseArgs inputsID inputs
    if null body
        then State.connectMonadic outputID
        else do
            mapM_ (buildNode False True Nothing) $ init body
            buildOutput outputID $ last body
    finalize
expr2graph _ = left "expr2graph: Unsupported Expr type"


prepareInputsOutputs :: AST.ID -> AST.ID -> GBPass (Node.ID, Node.ID)
prepareInputsOutputs functionID funOutputID = do
    let inputsID = - functionID
        outputID = - funOutputID
    State.insNode (inputsID, Node.Inputs)
    State.insNode (outputID, Node.Outputs)
    return (inputsID, outputID)


finalize :: GBPass (Graph, PropertyMap)
finalize = do g  <- State.getGraph
              pm <- State.getPropertyMap
              return (g, pm)


parseArgs :: Node.ID -> [Expr] -> GBPass ()
parseArgs inputsID inputs = do
    let numberedInputs = zip inputs [0..]
    mapM_ (parseArg inputsID) numberedInputs


parseArg :: Node.ID -> (Expr, Int) -> GBPass ()
parseArg inputsID (input, no) = case input of
    Expr.Arg _ pat _ -> do [p] <- buildPat pat
                           State.addToNodeMap p (inputsID, Port.Num no)
    _                -> left "parseArg: Wrong Expr type"


buildOutput :: Node.ID -> Expr -> GBPass ()
buildOutput outputID expr = do
    case expr of
        Expr.Assignment {} -> void $ buildNode False True Nothing expr
        Expr.Tuple _ items -> connectArgs True  True Nothing outputID items 0
        Expr.Var {}        -> connectArg  True  True Nothing outputID (expr, 0)
        _                  -> connectArg  False True Nothing outputID (expr, 0)
    State.connectMonadic outputID


buildNode :: Bool -> Bool -> Maybe String -> Expr -> GBPass AST.ID
buildNode astFolded monadicBind outName expr = case expr of
    Expr.Accessor i name dst  -> do let node = Node.Expr name (genName name i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectArg True True Nothing  i (dst, 0)
                                    connectMonadic i
                                    return i
    Expr.Assignment i pat dst -> do let patStr = Pat.lunaShow pat
                                    realPat <- isRealPat pat dst
                                    if realPat
                                        then do patIDs <- buildPat pat
                                                let node = Node.Expr ('=': patStr) (genName "pattern" i)
                                                State.insNodeWithFlags (i, node) astFolded assignment
                                                case patIDs of
                                                   [patID] -> State.addToNodeMap patID (i, Port.All)
                                                   _       -> mapM_ (\(n, patID) -> State.addToNodeMap patID (i, Port.Num n)) $ zip [0..] patIDs
                                                dstID <- buildNode True True Nothing dst
                                                State.connect dstID i 0
                                                connectMonadic i
                                                return i
                                        else do [p] <- buildPat pat
                                                j <- buildNode False True (Just patStr) dst
                                                State.addToNodeMap p (j, Port.All)
                                                return j
    Expr.App       _ src args -> do srcID <- buildNode astFolded False outName src
                                    s     <- State.gvmNodeMapLookUp srcID
                                    case s of
                                       Just (srcNID, _) -> connectArgs True True Nothing srcNID args 1
                                       Nothing          -> return ()
                                    connectMonadic srcID
                                    return srcID
    Expr.Infix i name src dst -> do let node = Node.Expr name (genName name i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectArg True True Nothing i (src, 0)
                                    connectArg True True Nothing i (dst, 1)
                                    connectMonadic i
                                    return i
    Expr.Var       i name     -> do isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp i
                                    if astFolded && isBound
                                        then return i
                                        else do let node = Node.Expr name (genName name i)
                                                State.addNode i Port.All node astFolded assignment
                                                connectMonadic i
                                                return i
    Expr.Con       i name     -> do let node = Node.Expr name (genName name i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectMonadic i
                                    return i
    Expr.Lit       i lvalue   -> do let litStr = Lit.lunaShow lvalue
                                        node = Node.Expr litStr (genName litStr i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectMonadic i
                                    return i
    Expr.Tuple     i items    -> do let node = Node.Expr "Tuple" (genName "tuple" i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectArgs True True Nothing i items 0
                                    connectMonadic i
                                    return i
    Expr.List _ [Expr.RangeFromTo {}] -> showAndAddNode
    Expr.List _ [Expr.RangeFrom   {}] -> showAndAddNode
    Expr.List      i items    -> do let node = Node.Expr "List" (genName "list" i)
                                    State.addNode i Port.All node astFolded assignment
                                    connectArgs True True Nothing i items 0
                                    connectMonadic i
                                    return i
    Expr.Native    i segments -> do let node = Node.Expr (showNative expr) (genName "native" i)
                                    State.addNode i Port.All node astFolded assignment
                                    let isNativeVar (Expr.NativeVar {}) = True
                                        isNativeVar _                   = False

                                        vars = filter isNativeVar segments
                                    connectArgs True True Nothing i vars 0
                                    connectMonadic i
                                    return i
    Expr.NativeVar i name     -> do isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp i
                                    if astFolded && isBound
                                        then return i
                                        else do let node = Node.Expr name (genName name i)
                                                State.addNode i Port.All node astFolded assignment
                                                connectMonadic i
                                                return i
    Expr.Wildcard  i          -> left $ "GraphBuilder: Unexpected Expr.Wildcard with id=" ++ show i
    _                         -> showAndAddNode
    where
        connectMonadic i = when monadicBind $ State.connectMonadic i
        assignment       = Maybe.isJust outName
        genName base num = Maybe.fromMaybe (OutputName.generate base num) outName

        showAndAddNode = do let i = expr ^. Expr.id
                                name = showExpr expr
                                node = Node.Expr name (genName name i)
                            State.addNode i Port.All node astFolded assignment
                            connectMonadic i
                            return i


buildArg :: Bool -> Bool -> Maybe String -> Expr -> GBPass (Maybe AST.ID)
buildArg astFolded monadicBind outName expr = case expr of
    Expr.Wildcard _ -> return Nothing
    _               -> Just <$> buildNode astFolded monadicBind outName expr


connectArgs :: Bool -> Bool -> Maybe String -> AST.ID -> [Expr] -> Int ->  GBPass ()
connectArgs astFolded monadicBind outName dstID exprs start =
    mapM_ (connectArg astFolded monadicBind outName dstID) $ zip exprs [start..]


connectArg :: Bool -> Bool -> Maybe String -> AST.ID -> (Expr, InPort) -> GBPass ()
connectArg astFolded monadicBind outName dstID (expr, dstPort) = do
    msrcID <- buildArg astFolded monadicBind outName expr
    case msrcID of
        Nothing    -> return ()
        Just srcID -> State.connect srcID dstID dstPort


isRealPat :: Pat -> Expr -> GBPass Bool
isRealPat pat dst = do
    isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp (dst ^. Expr.id)
    return $ case (pat, dst, isBound) of
        (Pat.Var {}, Expr.Var {}, True) -> True
        (Pat.Var {}, _          , _   ) -> False
        _                               -> True


buildPat :: Pat -> GBPass [AST.ID]
buildPat p = case p of
    Pat.Var      i _      -> return [i]
    Pat.Lit      i _      -> return [i]
    Pat.Tuple    _ items  -> List.concat <$> mapM buildPat items
    Pat.Con      i _      -> return [i]
    Pat.App      _ _ args -> List.concat <$> mapM buildPat args
    Pat.Typed    _ pat _  -> buildPat pat
    Pat.Wildcard i        -> return [i]


showExpr :: Expr -> String
showExpr expr = case expr of
    --Expr.Accessor     _ name     dst
    --Expr.App          _ src      args
    --Expr.AppCons_     _ args
    --Expr.Assignment   _ pat      dst
    --Expr.RecordUpdate _ name     selectors expr
    --Expr.Data         _ cls      cons      classes methods
    --Expr.ConD         _ name     fields
    --Expr.Con          _ name
    --Expr.Function     _ path     name      inputs  output  body
    --Expr.Import       _ path     target    rename
    --Expr.Infix        _ name     src       dst
    Expr.List         _ items        -> "[" ++ List.intercalate ", " (map showExpr items) ++ "]"
    Expr.Lit          _ lvalue       -> Lit.lunaShow lvalue
    --Expr.Tuple        _ items        -> "{" ++ List.intercalate ", " (map showExpr items) ++ "}"
    --Expr.Typed        _ cls      expr
    --Expr.Var          _ name         -> name
    Expr.Wildcard     _              -> "_"
    Expr.RangeFromTo  _ start    end -> showExpr start ++ ".." ++ showExpr end
    Expr.RangeFrom    _ start        -> showExpr start ++ ".."
    --Expr.Field        _ name     cls       value
    --Expr.Arg          _ pat      value
    --Expr.Native       _ segments     -> "```" ++ concatMap showExpr segments ++ "```"
    --Expr.NativeCode   _ code         -> code
    --Expr.NativeVar    _ name         -> "#{" ++ name ++ "}"
    --Expr.Case         _ expr     match
    --Expr.Match        _ pat      body


showNative :: Expr -> String
showNative native = case native of
    Expr.Native       _ segments     -> "```" ++ concatMap showNative segments ++ "```"
    Expr.NativeCode   _ code         -> code
    Expr.NativeVar    _ _            -> "#{}"
    _                                -> Prelude.error $ "Graph.Builder.Builder.showNative: Not a native: " ++ show native
