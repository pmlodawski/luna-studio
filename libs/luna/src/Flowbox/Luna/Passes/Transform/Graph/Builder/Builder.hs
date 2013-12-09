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
import qualified Data.List as List

import           Flowbox.Prelude                                   hiding (error, mapM, mapM_)
import           Flowbox.Luna.Data.AliasAnalysis                     (AA)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import           Flowbox.Luna.Data.AST.Lit                           (Lit)
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import           Flowbox.Luna.Data.AST.Pat                           (Pat)
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import           Flowbox.Luna.Data.Graph.Edge                        (Edge(Edge))
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import           Flowbox.Luna.Data.Graph.Node                        (Node)
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Pass                            (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Builder.State   (GBState)
import           Flowbox.System.Log.Logger                           
import qualified Flowbox.Luna.Data.Graph.Flags                     as Flags
import qualified Flowbox.Luna.Data.Attributes                      as Attributes



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"


type GBMonad m = PassMonad GBState m


run :: PassMonad s m => AA -> Expr -> Pass.Result m Graph
run aa = (Pass.run_ (Pass.Info "GraphBuilder") $ State.make aa) . expr2graph


expr2graph :: GBMonad m => Expr -> Pass.Result m Graph
expr2graph expr = case expr of
    Expr.Function i path name inputs output body -> do parseArgs inputs
                                                       mapM_ buildExpr body
                                                       s <- get
                                                       logger warning $ show s
                                                       State.getGraph
    _                                            -> fail "expr2graph: Unsupported Expr type"


parseArgs :: GBMonad m => [Expr] -> Pass.Result m ()
parseArgs inputs = do
    let numberedInputs = zip inputs [0..]
    mapM_ parseArg numberedInputs


parseArg :: GBMonad m => (Expr, Int) -> Pass.Result m ()
parseArg (input, no) = case input of
    Expr.Arg _ pat _ -> do ([p], _) <- buildPat pat
                           State.addToMap p (Graph.inputsID, Just no)
    _                -> fail "parseArg: Wrong Expr type"


buildExpr :: GBMonad m => Expr -> Pass.Result m AST.ID
buildExpr expr = case expr of
    Expr.Accessor   i name dst -> do dstID <- buildExpr dst
                                     (dstNID, dstPort) <- State.aaNodeMapLookUp dstID
                                     accNID <- State.insNewNode $ Node.Expr name (Just expr) dummyFlags dummyAttrs
                                     State.addToMap i (accNID, Nothing)
                                     State.connect dstNID accNID $ Edge dstPort 0
                                     return i
    Expr.Assignment i pat dst  -> do (patIDs, patStr) <- buildPat pat
                                     dstID <- buildExpr dst
                                     (dstNID, dstPort) <- State.aaNodeMapLookUp dstID
                                     patNID <- State.insNewNode $ Node.Expr ('=': patStr) (Just expr) dummyFlags dummyAttrs
                                     case patIDs of 
                                        [patID] -> State.addToMap patID (patNID, Nothing)
                                        _       -> mapM_ (\(n, patID) -> State.addToMap patID (patNID, Just n)) $ zip [0..] patIDs
                                     State.connect dstNID patNID $ Edge dstPort 0
                                     return dummyValue
    Expr.App        i src args -> do srcID <- buildExpr src
                                     (srcNID, _) <- State.aaNodeMapLookUp srcID
                                     argIDs <- mapM buildExpr args
                                     argNIDsP <- mapM State.aaNodeMapLookUp argIDs
                                     let numberedArgNIDsP = zip [1..] argNIDsP
                                     mapM_ (\(no, (argNID, p)) -> State.connect argNID srcNID $ Edge p no) numberedArgNIDsP
                                     return srcID
    Expr.Var        i _        -> do return i
    Expr.Lit        i lvalue   -> do (litID, litStr) <- buildLit lvalue
                                     litNID <- State.insNewNode $ Node.Expr litStr (Just expr) dummyFlags dummyAttrs
                                     State.addToMap litID (litNID, Nothing)
                                     return litID
    --Expr.Arg {}                -> dummyInsNewNode "dummy_expr_Arg"
    --_                          -> dummyInsNewNode "dummy_expr_Other"
    

expr2String :: GBMonad m => Expr -> Pass.Result m String
expr2String expr = case expr of
    Expr.Accessor _ name dst -> do dstStr <- expr2String dst
                                   return $ dstStr ++ "." ++ name
    Expr.Var      _ name     -> return name


buildPat :: GBMonad m => Pat -> Pass.Result m ([AST.ID], String)
buildPat pat = case pat of
    Pat.Var      i name     -> return ([i], name)
    Pat.Lit      i value    -> do (litID, litStr) <- buildLit value
                                  return ([i], litStr)
    Pat.Tuple    i items    -> do itemsStr <- mapM buildPat items
                                  let ids  = List.concat $ map fst itemsStr
                                      strs = map snd itemsStr
                                  return (ids, "{" ++ (List.intercalate ", " strs) ++ "}")
    Pat.Con      i name     -> return ([i], name)
    Pat.App      i src args -> do argsStr <- mapM buildPat args
                                  let ids  = List.concat $ map fst argsStr
                                      strs = map snd argsStr
                                  (srcID, srcStr) <- buildPat src
                                  return (ids, srcStr ++ " " ++ (List.intercalate " " strs))
    Pat.Typed    i pat cls  -> return $ ([i], dummyString "pattern_Typed")
    Pat.Wildcard i          -> return ([i], "_")


buildLit :: GBMonad m => Lit -> Pass.Result m (AST.ID, String)
buildLit lit = pure $ case lit of 
    Lit.Char    i char -> (i, [char])
    Lit.String  i str  -> (i, str)
    Lit.Integer i str  -> (i, str)
    Lit.Float   i str  -> (i, str)


-- REMOVE ME --
dummyInsNewNode name = State.insNewNode $ Node.Expr name Nothing dummyFlags dummyAttrs
dummyValue = (-1)
dummyNothing = Nothing
dummyFlags = Flags.empty
dummyAttrs = Attributes.empty
dummyString s = "dummy_" ++ s
--------------