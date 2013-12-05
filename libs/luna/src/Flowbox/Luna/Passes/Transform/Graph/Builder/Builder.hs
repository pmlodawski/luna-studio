---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.Builder where

import           Control.Applicative                                 
import           Control.Monad.State                                 
import qualified Data.Map                                          as Map
import           Data.Map                                            (Map)

import           Flowbox.Prelude                                   hiding (error, mapM_)
import           Flowbox.Luna.Data.AliasAnalysis                     (AA)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import           Flowbox.Luna.Data.AST.Pat                           (Pat)
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import           Flowbox.Luna.Data.Graph.Edge                        (Edge(Edge))
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import           Flowbox.Luna.Data.Graph.Node                        (Node)
import           Flowbox.Luna.Data.Graph.Port                        (Port)
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Pass                            (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Builder.State   (GBState)
import           Flowbox.System.Log.Logger                           
import qualified Flowbox.Luna.Data.Graph.Flags as Flags
import qualified Flowbox.Luna.Data.Attributes  as Attributes


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"


type GBMonad m = PassMonad GBState m


run :: PassMonad s m => AA -> Expr -> Pass.Result m Graph
run aa = (Pass.run_ (Pass.Info "GraphBuilder") $ State.make aa) . expr2graph


expr2graph :: GBMonad m => Expr -> Pass.Result m Graph
expr2graph expr = case expr of
    Expr.Function i path name inputs output body -> do parseArgs inputs
                                                       Expr.traverseM_ buildExpr pure pure pure expr
                                                       s <- get
                                                       logger warning $ show s
                                                       State.getGraph
    _                                            -> fail "expr2graph: Unsupported Expr type"


parseArgs :: GBMonad m => [Expr] -> Pass.Result m ()
parseArgs inputs = do
    let numberedInputs = zip inputs [0..]
    mapM_ parseArg numberedInputs


parseArg :: GBMonad m => (Expr, Port) -> Pass.Result m ()
parseArg (input, no) = case input of
    Expr.Arg i _ _ -> State.addToMap i (Graph.inputsID, no)
    _              -> fail "parseArg: Wrong Expr type"


buildExpr :: GBMonad m => Expr -> Pass.Result m Node.ID
buildExpr expr = case expr of
    Expr.Accessor   i name dst -> State.insNewNode $ Node.Expr name (Just expr) dummyFlags dummyAttrs
    Expr.Assignment i pat dst  -> do p <- buildPat pat
                                     d <- buildExpr dst
                                     State.connect p d $ Edge dummyNothing 0
                                     pure dummyValue
    Expr.App        i src args -> dummyInsNewNode "dummy_expr_App"
    _                          -> dummyInsNewNode "dummy_expr_Other"
    

buildPat :: GBMonad m => Pat -> Pass.Result m Node.ID
buildPat pat = case pat of
    Pat.Var      i name     -> dummyInsNewNode "dummy_pattern_Var"
    Pat.Lit      i value    -> dummyInsNewNode "dummy_pattern_Lit"
    Pat.Tuple    i items    -> dummyInsNewNode "dummy_pattern_Tuple"
    Pat.Con      i name     -> dummyInsNewNode "dummy_pattern_Con"
    Pat.App      i src args -> dummyInsNewNode "dummy_pattern_App"
    Pat.Typed    i pat cls  -> dummyInsNewNode "dummy_pattern_Typed"
    Pat.Wildcard i          -> dummyInsNewNode "dummy_pattern_Wildcard"


-- REMOVE ME --
dummyInsNewNode name = State.insNewNode $ Node.Expr name Nothing dummyFlags dummyAttrs
dummyValue = (-1)
dummyNothing = Nothing
dummyFlags = Flags.empty
dummyAttrs = Attributes.empty
--------------