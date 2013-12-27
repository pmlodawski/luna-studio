---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Luna.Passes.Transform.Graph.Builder.Builder where

import           Control.Applicative
import           Control.Monad.State
import qualified Data.List           as List

import           Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap    (GeneralVarMap)
import           Flowbox.Luna.Data.AST.Expr                        (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import           Flowbox.Luna.Data.AST.Pat                         (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import           Flowbox.Luna.Data.Graph.Graph                     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import qualified Flowbox.Luna.Data.Graph.Port                      as Port
import           Flowbox.Luna.Data.PropertyMap                     (PropertyMap)
import           Flowbox.Luna.Passes.Pass                          (PassMonad)
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Transform.Graph.Builder.State (GBState)
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.State as State
import           Flowbox.Prelude                                   hiding (error, mapM, mapM_)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"


type GBMonad m = PassMonad GBState m


run :: PassMonad s m => GeneralVarMap -> PropertyMap -> Expr -> Pass.Result m (Graph, PropertyMap)
run gvm pm = (Pass.run_ (Pass.Info "GraphBuilder") $ State.make gvm pm) . expr2graph


expr2graph :: GBMonad m => Expr -> Pass.Result m (Graph, PropertyMap)
expr2graph expr = case expr of
    Expr.Function _ _ _ inputs _ body -> do parseArgs inputs
                                            mapM_ (buildNode False Nothing) body
                                            g <- State.getGraph
                                            pm <- State.getPropertyMap
                                            return (g, pm)
    _                                 -> fail "expr2graph: Unsupported Expr type"


parseArgs :: GBMonad m => [Expr] -> Pass.Result m ()
parseArgs inputs = do
    let numberedInputs = zip inputs [0..]
    mapM_ parseArg numberedInputs


parseArg :: GBMonad m => (Expr, Int) -> Pass.Result m ()
parseArg (input, no) = case input of
    Expr.Arg _ pat _ -> do [p] <- buildPat pat
                           State.addToNodeMap p (Graph.inputsID, Port.Num no)
    _                -> fail "parseArg: Wrong Expr type"


buildNode :: GBMonad m => Bool -> Maybe String -> Expr -> Pass.Result m AST.ID
buildNode astFolded outName expr = case expr of
    Expr.Accessor   i name dst -> do dstID  <- buildNode True Nothing dst
                                     let node = Node.Expr name (genName name i)
                                     State.addNode i Port.All node astFolded noAssignment
                                     State.connect dstID i 0
                                     return i
    Expr.Assignment i pat dst  -> do let patStr = Pat.lunaShow pat
                                     if isRealPat pat
                                         then do patIDs <- buildPat pat
                                                 let node = Node.Expr ('=': patStr) (genName "pattern" i)
                                                 State.insNode (i, node) astFolded noAssignment
                                                 case patIDs of
                                                    [patID] -> State.addToNodeMap patID (i, Port.All)
                                                    _       -> mapM_ (\(n, patID) -> State.addToNodeMap patID (i, Port.Num n)) $ zip [0..] patIDs
                                                 dstID <- buildNode True Nothing dst
                                                 State.connect dstID i 0
                                                 return dummyValue
                                         else do [p] <- buildPat pat
                                                 j <- buildNode False (Just patStr) dst
                                                 State.addToNodeMap p (j, Port.All)
                                                 return dummyValue
    Expr.App        i src args -> do srcID       <- buildNode (astFolded || False) Nothing src
                                     (srcNID, _) <- State.gvmNodeMapLookUp srcID
                                     argIDs      <- mapM (buildNode True Nothing) args
                                     let numberedArgIDs = zip [1..] argIDs
                                     mapM_ (\(no, argID) -> State.connect argID srcNID no) numberedArgIDs
                                     return srcID
    Expr.Infix  i name src dst -> do srcID    <- buildNode True Nothing src
                                     dstID    <- buildNode True Nothing dst
                                     let node = Node.Expr name (genName name i)
                                     State.addNode i Port.All node astFolded noAssignment
                                     State.connect srcID i 0
                                     State.connect dstID i 1
                                     return i
    Expr.Var        i name     -> if astFolded
                                     then return i
                                     else do let node = Node.Expr name (genName name i)
                                             State.addNode i Port.All node astFolded noAssignment
                                             return i
    Expr.Con        i name     -> do let node = Node.Expr name (genName name i)
                                     State.addNode i Port.All node astFolded noAssignment
                                     return i
    Expr.Lit        i lvalue   -> do let litStr = Lit.lunaShow lvalue
                                         node = Node.Expr litStr (genName litStr i)
                                     State.addNode i Port.All node astFolded noAssignment
                                     return i
    Expr.Tuple      i items    -> do let node = Node.Expr "Tuple" (genName "tuple" i)
                                     State.addNode i Port.All node astFolded noAssignment
                                     itemIDs <- mapM (buildNode True Nothing) items
                                     let numberedItemsIDs = zip [0..] itemIDs
                                     mapM_ (\(no, argID) -> State.connect argID i no) numberedItemsIDs
                                     return i
    where
        genName base num = case outName of
            Nothing   -> base ++ "Result" ++ (show num)
            Just name -> name

        noAssignment = case outName of
            Nothing -> True
            Just _  -> False


isRealPat :: Pat -> Bool
isRealPat p = case p of
    Pat.Var {}-> False
    _         -> True


buildPat :: GBMonad m => Pat -> Pass.Result m [AST.ID]
buildPat p = case p of
    Pat.Var      i _      -> return [i]
    Pat.Lit      i _      -> return [i]
    Pat.Tuple    i items  -> List.concat <$> mapM buildPat items
    Pat.Con      i _      -> return [i]
    Pat.App      i _ args -> List.concat <$> mapM buildPat args
    Pat.Typed    i pat _  -> buildPat pat
    Pat.Wildcard i        -> return [i]


-- REMOVE ME --
dummyValue = (-1)
--------------
