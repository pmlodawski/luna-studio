---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE NoMonomorphismRestriction    #-}

module Luna.Pass.Transform.Graph.Parser.Parser where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.List                  as List
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe

import           Flowbox.Control.Error                  ((<??>))
import           Flowbox.Prelude                        hiding (folded, mapM, mapM_)
import           Flowbox.System.Log.Logger              hiding (error)
import           Luna.Data.ASTInfo                      (ASTInfo)
import qualified Luna.Data.ASTInfo                      as ASTInfo
import qualified Luna.Parser.Parser                     as Parser
import qualified Luna.Parser.Pragma                     as Pragma
import qualified Luna.Parser.State                      as Parser
import qualified Luna.Parser.Token                      as Tok
import           Luna.Pass.Transform.Graph.Parser.State (GPPass)
import qualified Luna.Pass.Transform.Graph.Parser.State as State
import           Luna.Syntax.Arg                        (Arg (Arg))
import qualified Luna.Syntax.Decl                       as Decl
import qualified Luna.Syntax.Enum                       as Enum
import qualified Luna.Syntax.Expr                       as Expr
import           Luna.Syntax.Graph.DefaultsMap          (DefaultsMap)
import           Luna.Syntax.Graph.Edge                 (Edge)
import qualified Luna.Syntax.Graph.Edge                 as Edge
import           Luna.Syntax.Graph.Graph                (Graph)
import qualified Luna.Syntax.Graph.Graph                as Graph
import qualified Luna.Syntax.Graph.Node                 as Node
import           Luna.Syntax.Graph.Node.Expr            (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr            as NodeExpr
import qualified Luna.Syntax.Graph.Node.MultiPart       as MultiPart
import qualified Luna.Syntax.Graph.Node.OutputPat       as OutputPat
import           Luna.Syntax.Graph.Node.Position        (Position)
import qualified Luna.Syntax.Graph.Node.StringExpr      as StringExpr
import           Luna.Syntax.Graph.Port                 (DstPortP (DstPort))
import qualified Luna.Syntax.Graph.Port                 as Port
import           Luna.Syntax.Graph.Tag                  (TDecl, TExpr, TPat, Tag)
import qualified Luna.Syntax.Graph.Tag                  as Tag
import           Luna.Syntax.Label                      (Label (Label))
import qualified Luna.Syntax.Label                      as Label
import qualified Luna.Syntax.Name.Pattern               as Pattern
import qualified Luna.Syntax.Pat                        as Pat
import qualified Luna.System.Pragma.Store               as Pragma
import           Luna.System.Session                    as Session
import qualified Luna.Util.Label                        as Label



type V = ()


logger :: Logger
logger = getLogger $moduleName


run :: MonadIO m => Graph Tag V -> TDecl V -> ASTInfo -> EitherT State.Error m (TDecl V, ASTInfo)
run graph ldecl astInfo = evalStateT (func2graph ldecl) $ State.mk graph astInfo


func2graph :: TDecl V -> GPPass V m (TDecl V, ASTInfo)
func2graph decl@(Label l (Decl.Func funcDecl)) = do
    let sig = funcDecl ^. Decl.funcDeclSig
    graph <- State.getGraph
    mapM_ (parseNode sig) $ Graph.sort graph
    b  <- State.getBody
    mo <- State.getOutput
    inputsPos  <- State.getInputsPos
    outputsPos <- State.getOutputsPos
    highestID  <- State.getHighestID
    let body = reverse $ case mo of
                Nothing -> b
                Just o  -> o : b
        label = if Tag.isEmpty l
            then Tag.mkNode (highestID+1) inputsPos (Just outputsPos) l
            else l & Tag.position .~ inputsPos
                   & Tag.additionalPos .~ Just outputsPos
    (decl & Label.label .~ label
          & Label.element . Decl.funcDecl . Decl.funcDeclBody .~ body
          ,) <$> State.getASTInfo


parseNode :: Decl.FuncSig a e -> (Node.ID, Node.Node Tag V) -> GPPass V m ()
parseNode signature (nodeID, node) = case node of
    Node.Outputs defaults pos -> parseOutputs nodeID defaults pos
    Node.Inputs           pos -> parseInputs nodeID signature pos
    Node.Expr expr outputPat defaults pos groupInfo _ -> do
        State.reportID nodeID
        graph <- State.getGraph
        let lsuclData = Graph.lsuclData graph nodeID
            outDataEdges = map (view _3) lsuclData
        srcs <- getNodeSrcs nodeID defaults
        ast  <- groupExpr groupInfo . (Label.label %~ Tag.mkNode nodeID pos Nothing) <$> buildExpr expr srcs
        if  | Maybe.isJust outputPat -> do
                pat <- buildPat expr nodeID outDataEdges outputPat
                let assignment = Expr.Assignment pat ast
                addExprs nodeID pat
                State.addToBody =<< newLabel assignment
            | not (null outDataEdges) -> State.addToExprMap (nodeID, Port.mkSrcAll) $ return ast
            | otherwise ->  State.addToBody ast


groupExpr :: [Tag] -> TExpr v -> TExpr v
groupExpr []    texpr = texpr
groupExpr (h:t) texpr = groupExpr t $ Label h $ Expr.Grouped texpr


--buildExpr :: NodeExpr Tag V -> [TExpr V] -> GPPass V m (TExpr V)
buildExpr nodeExpr srcs = case nodeExpr of
    NodeExpr.ASTExpr expr -> return expr
    NodeExpr.MultiPart mp -> do let defArg = Expr.unnamed <$> newLabel' Expr.Wildcard
                                    (self, args) = case srcs of
                                        []    -> (newLabel' Expr.Wildcard, [])
                                        (h:t) -> (return h, map Expr.unnamed t)
                                acc <- State.withASTInfo $ MultiPart.toNamePat mp self args defArg
                                newLabel $ Expr.App acc
    NodeExpr.StringExpr strExpr -> case strExpr of
        StringExpr.List           -> newLabel $ Expr.List $ Expr.SeqList srcs
        StringExpr.Tuple          -> newLabel $ Expr.Tuple srcs
        StringExpr.Pattern pat    -> lift $ Maybe.listToMaybe srcs <??> "GraphParser.buildExpr"
        _                         -> do
            let str = toString strExpr
            expr <- if isOperator str
                then newLabel $ Expr.Var $ Expr.Variable (fromString str) ()
                else do
                    astInfo <- State.getASTInfo
                    r <- Session.run def $ do
                        void   Parser.init
                        void $ Pragma.enable Pragma.orphanNames
                        let parserState = Parser.defState & Parser.info .~ astInfo
                        Parser.parseString str (Parser.exprParser parserState)
                    case r of
                        Left err -> lift $ left $ toString err
                        Right (lexpr, parserState) -> do
                            State.setASTInfo $ parserState ^. Parser.info
                            return $ Label.label . Tag.folded .~ True $ Label.replaceExpr Tag.fromEnumerated lexpr
            case (unwrap expr, srcs) of
                (Expr.Var (Expr.Variable vname _), h:t) -> do
                    let args = map Expr.unnamed t
                    acc <- newLabel $ Expr.Accessor (fromString $ toString $ unwrap vname) h
                    newLabel $ Expr.app acc args
                _ -> return expr

        --StringExpr.Pattern pat    -> parsePatNode     nodeID pat
        --StringExpr.Native  native -> parseNativeNode  nodeID native
        --_                         -> parseAppNode     nodeID $ StringExpr.toString str

isOperator :: String -> Bool
isOperator = all (`elem` Tok.opChars)


buildPat :: NodeExpr Tag V -> Node.ID -> [Edge] -> Maybe TPat -> GPPass V m TPat
buildPat nodeExpr nodeID edges = construct (List.nub $ map (unwrap . (^?! Edge.src)) edges)
    where
        construct [] (Just o) = return o
        construct []  _       = l . Pat.Grouped =<< l (Pat.Tuple [])
        construct [Port.All] (Just o@(Label _ (Pat.Var _))) = return o
        construct [Port.All] _                              = State.withASTInfo $ OutputPat.generate nodeExpr nodeID
        l = newLabel


addExprs :: Node.ID -> TPat -> GPPass V m ()
addExprs nodeID tpat = case tpat of
    Label _ (Pat.Var  vname) -> addToExprMap Port.mkSrcAll vname
    Label _ (Pat.Grouped gr) -> addExprs nodeID gr
    Label _ (Pat.Tuple   tp) -> add 0 tp
    _                        -> return ()
    where
        addToExprMap port vname = State.addToExprMap (nodeID, port)
                                $ newLabel' $ Expr.Var $ Expr.Variable vname ()
        add _ []    = return ()
        add i (h:t) = case h of
            Label _ (Pat.Var vname) -> addToExprMap (Port.mkSrc i) vname >> add (i + 1) t
            _                       -> add (i + 1) t


parseInputs :: Node.ID -> Decl.FuncSig a e -> Position -> GPPass V m ()
parseInputs nodeID funcSig position = do
    State.setInputsPos position
    mapM_ (parseArg nodeID) $ zip [0..] $ Pattern.args funcSig


parseArg :: Node.ID -> (Int, Arg a e) -> GPPass V m ()
parseArg nodeID (num, input) = case input of
    Arg (Label _                     (Pat.Var vname)    ) _ -> addVar vname
    Arg (Label _ (Pat.Typed (Label _ (Pat.Var vname)) _)) _ -> addVar vname
    _                                                       -> lift $ left "parseArg: Wrong Arg type"
    where addVar vname = State.addToExprMap (nodeID, Port.mkSrc num)
                       $ newLabel' $ Expr.Var $ Expr.Variable vname ()


parseOutputs :: Node.ID -> DefaultsMap Tag V -> Position -> GPPass V m ()
parseOutputs nodeID defaults position = do
    State.setOutputsPos position
    srcs    <- getNodeSrcs nodeID defaults
    inPorts <- State.inboundPorts nodeID
    case (srcs, map unwrap inPorts) of
        ([], _)               -> whenM doesLastStatementReturn $
                                   State.setOutput =<< mkTuple []
        ([src], [Port.Num 0]) -> State.setOutput =<< mkTuple [src]
        ([src], _           ) -> State.setOutput src
        _                     -> State.setOutput =<< mkTuple srcs
    where
        mkTuple = newLabel . Expr.Grouped <=< newLabel . Expr.Tuple

doesLastStatementReturn :: GPPass V m Bool
doesLastStatementReturn = do
    body <- State.getBody
    return $ case body of
        []                                 -> False
        (Label _ (Expr.Assignment {}) : _) -> False --TODO[PM] : check it
        _                                  -> True


getNodeSrcs :: Node.ID -> DefaultsMap Tag v -> GPPass v m [TExpr v]
getNodeSrcs nodeID defaults = do
    g <- State.getGraph
    connectedVars <- mapM (getVar . processEdge) $ Graph.lprelData g nodeID
    let defalutsExprs = map processDefault $ Map.toList defaults
        srcsMap = Map.union (Map.fromList connectedVars) (Map.fromList defalutsExprs)
    if Map.null srcsMap
        then return []
        else do let maxPort = fst $ Map.findMax srcsMap
                mapM (wildcardMissing . flip Map.lookup srcsMap) [0..maxPort]
    where
        processEdge (pNID, _, Edge.Data s (DstPort  Port.All   )) = (0, (pNID, s))
        processEdge (pNID, _, Edge.Data s (DstPort (Port.Num d))) = (d, (pNID, s))

        processDefault (DstPort  Port.All   , expr) = (0, expr)
        processDefault (DstPort (Port.Num d), expr) = (d, expr)

        getVar (i, key) = (i,) <$> State.exprMapLookup key

        wildcardMissing Nothing  = newLabel Expr.Wildcard
        wildcardMissing (Just e) = return e


newLabel :: a -> GPPass v m (Label Tag a)
newLabel = State.withASTInfo . newLabel'

newLabel' :: a -> State ASTInfo (Label Tag a)
newLabel' a = do
    n <- ASTInfo.incID <$> get
    put n
    return $ Label (Enum.tag $ n ^. ASTInfo.lastID) a
