---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.Parser where

import Control.Monad.State

import           Flowbox.Luna.Data.AST.Expr                            (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                            as Expr
import qualified Flowbox.Luna.Data.AST.Pat                             as Pat
import qualified Flowbox.Luna.Data.Attributes                          as Attributes
import           Flowbox.Luna.Data.Graph.Graph                         (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                         as Graph
import           Flowbox.Luna.Data.Graph.Node                          (Node)
import qualified Flowbox.Luna.Data.Graph.Node                          as Node
import qualified Flowbox.Luna.Data.Graph.Port                          as Port
import           Flowbox.Luna.Data.PropertyMap                         (PropertyMap)
import qualified Flowbox.Luna.Data.PropertyMap                         as PropertyMap
import           Flowbox.Luna.Passes.Pass                              (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                              as Pass
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.State       as IDFixer
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser    as Parser
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes        as Attributes
import qualified Flowbox.Luna.Passes.Transform.Graph.Defaults.Defaults as Defaults
import           Flowbox.Luna.Passes.Transform.Graph.Parser.State      (GPState)
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.State      as State
import           Flowbox.Prelude                                       hiding (error, folded, mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.Parser"


type GPMonad m = PassMonad GPState m


run :: PassMonad s m => Graph -> PropertyMap -> Expr -> Pass.Result m Expr
run gr pm = (Pass.run_ (Pass.Info "GraphParser") $ State.make gr pm) . graph2expr


graph2expr :: GPMonad m => Expr -> Pass.Result m Expr
graph2expr expr = do
    let inputs = expr ^. Expr.inputs
    graph <- State.getGraph
    mapM_ (parseNode inputs) $ Graph.topsortl graph
    body <- reverse <$> State.getBody
    return (Expr.body .~ body $ expr)


parseNode :: GPMonad m => [Expr] ->  (Node.ID, Node) -> Pass.Result m ()
parseNode inputs (nodeID, node) = do
    case node of
        Node.Expr expr _  -> parseExprNode    nodeID expr
        Node.Inputs       -> parseInputsNode  nodeID inputs
        Node.Outputs      -> parseOutputsNode nodeID


parseExprNode :: GPMonad m => Node.ID -> String -> Pass.Result m ()
parseExprNode nodeID expr = case expr of
    '=':pat -> parsePatNode   nodeID pat
    '~':_   -> parseInfixNode nodeID expr
    _       -> do generated <- isGenerated nodeID 
                  if generated
                     then fail "GraphParser: Not implemented for generated nodes" --parseAppNode IDFixer.unknownID expr
                     else parseAppNode nodeID expr


parseInputsNode :: GPMonad m => Node.ID -> [Expr] -> Pass.Result m ()
parseInputsNode nodeID inputs = do
    mapM_ (parseArg nodeID) $ zip [0..] inputs


parseArg :: State.GPStateM m => Node.ID  -> (Int, Expr) -> m ()
parseArg nodeID (num, input) = case input of
    Expr.Arg _ (Pat.Var _ name) _ -> State.addToNodeMap (nodeID, Port.Num num) $ Expr.Var IDFixer.unknownID name
    _ -> fail "parseArg: Wrong Arg type"


parseOutputsNode :: GPMonad m => Node.ID -> Pass.Result m ()
parseOutputsNode nodeID = do
    srcs <- State.getNodeSrcs nodeID
    let e = case srcs of
                [s] -> s
                _   -> Expr.Tuple IDFixer.unknownID srcs
    State.addToBody e


parsePatNode :: GPMonad m => Node.ID -> String -> Pass.Result m ()
parsePatNode nodeID pat = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [s] -> do p <- case Parser.parsePattern pat IDFixer.unknownID of
                            Left  er     -> fail $ show er
                            Right (p, _) -> return p
                  let e = Expr.Assignment nodeID p s
                  State.addToNodeMap (nodeID, Port.All) e
                  State.addToBody e
        _      -> fail "parsePatNode: Wrong Pat arguments"


parseInfixNode :: GPMonad m => Node.ID -> String -> Pass.Result m ()
parseInfixNode nodeID inf = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        [a, b] -> do let e = Expr.Infix nodeID inf a b
                     addExpr nodeID e
        _      -> fail "parseInfixNode: Wrong Infix arguments"


parseAppNode :: GPMonad m => Node.ID -> String -> Pass.Result m ()
parseAppNode nodeID app = do
    srcs <- State.getNodeSrcs nodeID
    case srcs of
        []  -> case Parser.parseExpr app nodeID of
                    Left  er     -> fail $ show er
                    Right (e, _) -> addExpr nodeID e
        [f] -> do let e   = Expr.Accessor nodeID app f
                  addExpr nodeID e
        f:t -> do let acc = Expr.Accessor nodeID app f
                      e   = Expr.App      IDFixer.unknownID acc t
                  addExpr nodeID e


addExpr :: GPMonad m => Node.ID -> Expr -> Pass.Result m ()
addExpr nodeID e = do
    gr <- State.getGraph
    folded        <- hasFlag nodeID Attributes.astFolded
    noAssignement <- hasFlag nodeID Attributes.astNoAssignment
    if folded
        then State.addToNodeMap (nodeID, Port.All) e
        else if noAssignement && (Graph.outdeg gr nodeID == 0)
            then do State.addToBody e
            else do outName <- State.getNodeOutputName nodeID
                    let p = Pat.Var IDFixer.unknownID outName
                        v = Expr.Var IDFixer.unknownID outName
                        a = Expr.Assignment IDFixer.unknownID p e
                    State.addToNodeMap (nodeID, Port.All) v
                    State.addToBody a


hasFlag :: GPMonad m => Node.ID -> String -> Pass.Result m Bool
hasFlag nodeID flag = do
    pm <- State.getPropertyMap
    case PropertyMap.get nodeID Attributes.luna flag pm of
        Just "True" -> return True
        _           -> return False


isGenerated :: GPMonad m => Node.ID -> Pass.Result m Bool
isGenerated nodeID = Defaults.isGenerated nodeID <$> State.getPropertyMap
