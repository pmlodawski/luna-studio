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
import qualified Data.List           as List

import           Flowbox.Prelude                                   hiding (error, mapM, mapM_)
import           Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap    (GeneralVarMap)
import qualified Flowbox.Luna.Data.Attributes                      as Attributes
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                        (Expr)
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import           Flowbox.Luna.Data.AST.Pat                         (Pat)
import qualified Flowbox.Luna.Data.AST.Utils                       as AST
import           Flowbox.Luna.Data.Graph.Graph                     (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                     as Graph
import qualified Flowbox.Luna.Data.Graph.Node                      as Node
import qualified Flowbox.Luna.Data.Graph.Port                      as Port
import qualified Flowbox.Luna.Data.Graph.Properties                as Properties
import           Flowbox.Luna.Data.Graph.Properties                (Properties)
import qualified Flowbox.Luna.Passes.Pass                          as Pass
import           Flowbox.Luna.Passes.Pass                          (PassMonad)
import qualified Flowbox.Luna.Passes.Transform.Graph.Attributes    as Attributes
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.State as State
import           Flowbox.Luna.Passes.Transform.Graph.Builder.State   (GBState)
import           Flowbox.System.Log.Logger                           



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Builder.Builder"


type GBMonad m = PassMonad GBState m


run :: PassMonad s m => GeneralVarMap -> Expr -> Pass.Result m Graph
run gvm = (Pass.run_ (Pass.Info "GraphBuilder") $ State.make gvm) . expr2graph


expr2graph :: GBMonad m => Expr -> Pass.Result m Graph
expr2graph expr = case expr of
    Expr.Function _ _ _ inputs _ body -> do parseArgs inputs
                                            mapM_ (buildNode False Nothing) body
                                            State.getGraph
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
                                     let node = Node.Expr name (genName name i) $ addAttr astFolded dummyProperties 
                                     State.addNode i Port.All node
                                     State.connectAST dstID i 0
                                     return i
    Expr.Assignment i pat dst  -> do let patStr = Pat.lunaShow pat
                                     if isRealPat pat
                                         then do patIDs <- buildPat pat
                                                 let node = Node.Expr ('=': patStr) (genName "pattern" i) $ addAttr astFolded dummyProperties 
                                                 State.insNode (i, node)
                                                 case patIDs of 
                                                    [patID] -> State.addToNodeMap patID (i, Port.All)
                                                    _       -> mapM_ (\(n, patID) -> State.addToNodeMap patID (i, Port.Num n)) $ zip [0..] patIDs
                                                 dstID <- buildNode True Nothing dst
                                                 State.connectAST dstID i 0
                                                 return dummyValue
                                         else do [p] <- buildPat pat
                                                 j <- buildNode False (Just patStr) dst
                                                 State.addToNodeMap p (j, Port.All)
                                                 return dummyValue
    Expr.App        i src args -> do srcID       <- buildNode (astFolded || False) Nothing src
                                     (srcNID, _) <- State.gvmNodeMapLookUp srcID
                                     argIDs      <- mapM (buildNode True Nothing) args
                                     let numberedArgIDs = zip [1..] argIDs
                                     mapM_ (\(no, argID) -> State.connectAST argID srcNID no) numberedArgIDs
                                     return srcID
    Expr.Infix  i name src dst -> do srcID    <- buildNode True Nothing src 
                                     dstID    <- buildNode True Nothing dst
                                     let node = Node.Expr name (genName name i) $ addAttr astFolded dummyProperties 
                                     State.addNode i Port.All node
                                     State.connectAST srcID i 0
                                     State.connectAST dstID i 1
                                     return i
    Expr.Var        i name     -> if astFolded 
                                     then return i
                                     else do let node = Node.Expr name (genName name i) $ addAttr astFolded dummyProperties 
                                             State.addNode i Port.All node
                                             return i
    Expr.Con        i name     -> do let node = Node.Expr name (genName name i) $ addAttr astFolded dummyProperties 
                                     State.addNode i Port.All node
                                     return i
    Expr.Lit        i lvalue   -> do let litStr = Lit.lunaShow lvalue
                                         node = Node.Expr litStr (genName litStr i) $ addAttr astFolded dummyProperties 
                                     State.addNode i Port.All node
                                     return i
    where 
        genName base num = case outName of
            Nothing   -> "out" ++ (show num) ++ "_" ++ base
            Just name -> name


addAttr :: Bool -> Properties -> Properties
addAttr astFolded p = if astFolded 
    then p & Properties.attrs %~ Attributes.set Attributes.luna Attributes.astFolded Attributes.true
    else p
  


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
dummyProperties = Properties.empty
--------------