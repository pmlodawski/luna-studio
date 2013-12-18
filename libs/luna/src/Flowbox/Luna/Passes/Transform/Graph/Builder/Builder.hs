---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.Builder where

import           Control.Applicative                                 
import           Control.Monad.State                                 
import qualified Data.List as List

import           Flowbox.Control.Error
import           Flowbox.Prelude                                   hiding (error, mapM, mapM_)
import           Flowbox.Luna.Data.AliasAnalysis                     (AA)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import           Flowbox.Luna.Data.AST.Lit                           (Lit)
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import           Flowbox.Luna.Data.AST.Pat                           (Pat)
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import           Flowbox.Luna.Data.Graph.Edge                        (Edge(Edge))
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import           Flowbox.Luna.Data.Graph.Port                        (InPort)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import qualified Flowbox.Luna.Data.Graph.Properties                as Properties
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Pass                            (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Builder.State   (GBState)
import           Flowbox.System.Log.Logger                           



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"


type GBMonad m = PassMonad GBState m


run :: PassMonad s m => AA -> Expr -> Pass.Result m Graph
run aa = (Pass.run_ (Pass.Info "GraphBuilder") $ State.make aa) . expr2graph


expr2graph :: GBMonad m => Expr -> Pass.Result m Graph
expr2graph expr = case expr of
    Expr.Function i path name inputs output body -> do parseArgs inputs
                                                       mapM_ buildNode body
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


data Input = Name String
           | Var AST.ID


buildNode :: GBMonad m => Expr -> Pass.Result m AST.ID
buildNode expr = case expr of
    Expr.Accessor   i name dst -> do (dstIDs, mdstStr) <- buildExpr dst
                                     let accName = case mdstStr of
                                                        Just dstStr -> dstStr ++ "." ++ name
                                                        Nothing     -> name
                                     accNID <- State.addNode i Nothing $ Node.Expr accName (Just expr) dummyProperties
                                     case dstIDs of
                                        [] -> return ()
                                        _  -> connect accNID (0, (dstIDs, mdstStr))
                                     return i
    Expr.Assignment i pat dst  -> do dstID <- buildNode dst
                                     (dstNID, dstPort) <- State.aaNodeMapLookUp dstID
                                     patNID <- if isRealPattern pat
                                                  then do (patIDs, patStr) <- buildPat pat
                                                          patNID <- State.insNewNode $ Node.Expr ('=': patStr) (Just expr) dummyProperties
                                                          case patIDs of 
                                                             [patID] -> State.addToMap patID (patNID, Nothing)
                                                             _       -> mapM_ (\(n, patID) -> State.addToMap patID (patNID, Just n)) $ zip [0..] patIDs
                                                          return patNID
                                                  else do State.insNewNode $ Node.Expr "id" (Just expr) dummyProperties
                                                          -- TODO [PM] : lost information about output name
                                     State.connect dstNID patNID $ Edge dstPort 0
                                     return dummyValue
    Expr.App        i src args -> do (srcIDs, mname) <- buildExpr src
                                     argsInfo <- mapM buildExpr args

                                     name <- mname <?> "Expr.App without a name to call"
                                     appNID <- State.addNode i Nothing $ Node.Expr name (Just expr) dummyProperties
                                     let allArgsInfo = case srcIDs of 
                                                            [] -> argsInfo
                                                            _  -> (srcIDs, mname):argsInfo
                                     mapM_ (connect appNID) $ zip [0..] $ allArgsInfo
                                     return i
    --Expr.Infix  i name src dst -> do (srcID, _) <- buildExpr src
    --                                 (dstID, _) <- buildExpr dst
    --                                 (srcNID, srcP) <- State.aaNodeMapLookUp srcID
    --                                 (dstNID, dstP) <- State.aaNodeMapLookUp dstID
    --                                 infixNID <- State.addNode i Nothing $ Node.Expr name (Just expr) dummyProperties
    --                                 State.connect srcNID infixNID $ Edge srcP 0
    --                                 State.connect dstNID infixNID $ Edge dstP 1
    --                                 return i


connect :: GBMonad m => Node.ID -> (InPort, ([AST.ID], Maybe String)) -> Pass.Result m ()
connect dstNID (dstPort, (srcIDs, mname)) = case srcIDs of
    [] -> do name <- mname <?> "GraphBuilder: Could not connect!"
             State.addNodeDefault dstPort name dstNID 
    _  -> do srcIDsP <- mapM State.aaNodeMapLookUp srcIDs
             mapM_ (\(srcNID, p) -> State.connect srcNID dstNID $ Edge p dstPort) srcIDsP




buildExpr :: GBMonad m => Expr -> Pass.Result m ([AST.ID], Maybe String)
buildExpr expr = case expr of
    Expr.Accessor   i name dst -> do (dstIDs, mdstStr) <- buildExpr dst
                                     return $ case mdstStr of
                                                Nothing -> (dstIDs, Just name)
                                                Just n  -> (dstIDs, Just $ n ++ "." ++ name)
    Expr.Var        i _        -> do return ([i], Nothing)
    ----Expr.Assignment i pat dst  -> do (patIDs, patStr) <- buildPat pat
    ----                                 dstID <- buildExpr dst
    ----                                 (dstNID, dstPort) <- State.aaNodeMapLookUp dstID
    ----                                 patNID <- State.insNewNode $ Node.Expr ('=': patStr) (Just expr) dummyProperties
    ----                                 case patIDs of 
    ----                                    [patID] -> State.addToMap patID (patNID, Nothing)
    ----                                    _       -> mapM_ (\(n, patID) -> State.addToMap patID (patNID, Just n)) $ zip [0..] patIDs
    ----                                 State.connect dstNID patNID $ Edge dstPort 0
    ----                                 return dummyValue
    ----Expr.App        i src args -> do srcID <- buildExpr src
    ----                                 (srcNID, _) <- State.aaNodeMapLookUp srcID
    ----                                 argIDs <- mapM buildExpr args
    ----                                 argNIDsP <- mapM State.aaNodeMapLookUp argIDs
    ----                                 let numberedArgNIDsP = zip [1..] argNIDsP
    ----                                 mapM_ (\(no, (argNID, p)) -> State.connect argNID srcNID $ Edge p no) numberedArgNIDsP
    ----                                 return srcID
    ----Expr.Infix  i name src dst -> do srcID <- buildExpr src
    ----                                 dstID <- buildExpr dst
    ----                                 (srcNID, srcP) <- State.aaNodeMapLookUp srcID
    ----                                 (dstNID, dstP) <- State.aaNodeMapLookUp dstID
    ----                                 infixNID <- State.addNode i Nothing $ Node.Expr name (Just expr) dummyProperties
    ----                                 State.connect srcNID infixNID $ Edge srcP 0
    ----                                 State.connect dstNID infixNID $ Edge dstP 1
    ----                                 return i
    Expr.Con        i name     -> do return ([], Just name)
    Expr.Lit        i lvalue   -> do (_, litStr) <- buildLit lvalue
                                     return ([], Just litStr)


isRealPattern :: Pat -> Bool
isRealPattern pat = case pat of
    Pat.Var      {} -> False
    Pat.Wildcard {} -> False
    _               -> True


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
    Pat.Typed    i pat cls  -> do (patIDs, patStr) <- buildPat pat
                                  return $ (patIDs, patStr ++ " :: " ++ dummyString "pattern_Typed")
    Pat.Wildcard i          -> return ([i], "_")


buildLit :: GBMonad m => Lit -> Pass.Result m (AST.ID, String)
buildLit lit = pure $ case lit of 
    Lit.Char    i char -> (i, [char])
    Lit.String  i str  -> (i, str)
    Lit.Integer i str  -> (i, str)
    Lit.Float   i str  -> (i, str)


-- REMOVE ME --
dummyValue = (-1)
dummyProperties = Properties.empty
dummyString s = "dummy_" ++ s
--------------