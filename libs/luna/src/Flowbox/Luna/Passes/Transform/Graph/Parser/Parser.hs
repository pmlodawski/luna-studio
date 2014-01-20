---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import Control.Monad.State

import           Flowbox.Luna.Data.AST.Expr                                (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                                as Expr
import qualified Flowbox.Luna.Data.AST.Pat                                 as Pat
import qualified Flowbox.Luna.Data.Attributes                              as Attributes
import           Flowbox.Luna.Data.Graph.Graph                             (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                             as Graph
import           Flowbox.Luna.Data.Graph.Node                              (Node)
import qualified Flowbox.Luna.Data.Graph.Node                              as Node
import qualified Flowbox.Luna.Data.Graph.Port                              as Port
import           Flowbox.Luna.Data.PropertyMap                             (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap                             as PropertyMap
import           Flowbox.Luna.Passes.Pass                                  (Pass)
import qualified Flowbox.Luna.Passes.Pass                                  as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.State           as IDFixer
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser        as Parser
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState    as ParseState
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes            as Attributes
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State          (GPState)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State          as State
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import           Flowbox.Prelude                                           hiding (error, folded, mapM, mapM_)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer as Lexer



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


type GPPass result = Pass GPState result


run :: Graph -> PropertyMap -> Expr -> Pass.Result Expr
run gr pm = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr pm) . graph2expr


graph2expr :: Expr -> GPPass Expr
graph2expr expr = do
    let inputs = expr ^. Expr.inputs
    graph <- State.getGraph
    mapM_ (parseNode inputs) $ Graph.topsortl graph
    b <- State.getBody
    o <- State.getOutput
    let body = reverse $ o : b
    return (Expr.body .~ body $ expr)


parseNode :: [Expr] ->  (Node.ID, Node) -> GPPass ()
parseNode inputs (nodeID, node) = do
    case node of
        Node.Expr expr _  -> parseExprNode    nodeID expr
        Node.Inputs       -> parseInputsNode  nodeID inputs
        Node.Outputs      -> parseOutputsNode nodeID


parseExprNode :: Node.ID -> String -> GPPass ()
parseExprNode nodeID expr = if length expr == 1 && head expr `elem` Lexer.operators
    then parseInfixNode nodeID expr
    else case expr of
        "Tuple" -> parseTupleNode nodeID
        '=':pat -> parsePatNode   nodeID pat
        _       -> parseAppNode   nodeID expr


parseInputsNode :: Node.ID -> [Expr] -> GPPass ()
parseInputsNode nodeID = mapM_ (parseArg nodeID) . zip [0..]


parseArg :: Node.ID -> (Int, Expr) -> GPPass ()
parseArg nodeID (num, input) = case input of
    Expr.Arg _              (Pat.Var _ name)    _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var IDFixer.unknownID name
    Expr.Arg _ (Pat.Typed _ (Pat.Var _ name) _) _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var IDFixer.unknownID name
    _                                             -> fail "parseArg: Wrong Arg type"


parseOutputsNode :: Node.ID -> GPPass ()
parseOutputsNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    let e = case srcs of
                [s] -> s
                _   -> Expr.Tuple IDFixer.unknownID srcs
    State.setOutput e


parsePatNode :: Node.ID -> String -> GPPass ()
parsePatNode nodeID pat = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [s] -> do p <- case Parser.parsePattern pat $ ParseState.make IDFixer.unknownID of
                            Left  er     -> fail $ show er
                            Right (p, _) -> return p
                  let e = Expr.Assignment nodeID p s
                  State.addToNodeMap (nodeID, Port.All) e
                  State.addToBody e
        _      -> fail "parsePatNode: Wrong Pat arguments"


parseInfixNode :: Node.ID -> String -> GPPass ()
parseInfixNode nodeID inf = do
    srcs   <- State.getNodeSrcs nodeID
    let u = Expr.Wildcard IDFixer.unknownID
    (a, b) <- case srcs of
                    [a, b] -> return (a, b)
                    [a]    -> return (a, u)
                    []     -> return (u, u)
                    _      -> fail "parseInfixNode: Wrong Infix arguments"
    addExpr nodeID $ Expr.Infix nodeID inf a b


parseAppNode :: Node.ID -> String -> GPPass ()
parseAppNode nodeID app = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        []  -> case Parser.parseExpr app $ ParseState.make nodeID of
                    Left  er     -> fail $ show er
                    Right (e, _) -> addExpr nodeID e
        [f] -> do let e   = Expr.Accessor nodeID app f
                  addExpr nodeID e
        f:t -> do let acc = Expr.Accessor nodeID app f
                      e   = Expr.App      IDFixer.unknownID acc t
                  addExpr nodeID e


parseTupleNode :: Node.ID -> GPPass ()
parseTupleNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    let e = Expr.Tuple nodeID srcs
    addExpr nodeID e


addExpr :: Node.ID -> Expr -> GPPass ()
addExpr nodeID e = do
    gr <- State.getGraph
    folded         <- hasFlag nodeID Attributes.astFolded
    noAssignement  <- hasFlag nodeID Attributes.astNoAssignment
    defaultNodeGen <- hasFlag nodeID Attributes.defaultNodeGenerated
    if (folded || defaultNodeGen) && (Graph.outdeg gr nodeID <= 1)
        then State.addToNodeMap (nodeID, Port.All) e
        else if noAssignement && (Graph.outdeg gr nodeID == 0)
            then do State.addToBody e
            else do outName <- State.getNodeOutputName nodeID
                    let p = Pat.Var IDFixer.unknownID outName
                        v = Expr.Var IDFixer.unknownID outName
                        a = Expr.Assignment IDFixer.unknownID p e
                    State.addToNodeMap (nodeID, Port.All) v
                    State.addToBody a


hasFlag :: Node.ID -> String -> GPPass Bool
hasFlag nodeID flag = do
    pm <- State.getPropertyMap
    case PropertyMap.get nodeID Attributes.luna flag pm of
        Just "True" -> return True
        _           -> return False


isGenerated :: Node.ID -> GPPass Bool
isGenerated nodeID = Defaults.isGenerated nodeID <$> State.getPropertyMap
