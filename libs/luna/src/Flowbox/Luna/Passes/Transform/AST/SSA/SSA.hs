---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Passes.Transform.AST.SSA.SSA where

import Control.Applicative
import Control.Monad.State

import           Flowbox.Luna.Data.Analysis.Alias.Alias (AA)
import qualified Flowbox.Luna.Data.Analysis.Alias.Alias as AA
import qualified Flowbox.Luna.Data.AST.Expr             as Expr
import           Flowbox.Luna.Data.AST.Module           (Module)
import qualified Flowbox.Luna.Data.AST.Module           as Module
import           Flowbox.Luna.Data.AST.Pat              (Pat)
import qualified Flowbox.Luna.Data.AST.Pat              as Pat
import           Flowbox.Luna.Passes.Pass               (Pass)
import qualified Flowbox.Luna.Passes.Pass               as Pass
import           Flowbox.Prelude                        hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.SSA.SSA"


type SSAPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: AA -> Module -> Pass.Result Module
run vs = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaModule vs)


ssaModule :: AA -> Module -> SSAPass Module
ssaModule vs mod = Module.traverseM (ssaModule vs) (ssaExpr vs) pure ssaPat pure mod


ssaExpr :: AA -> Expr.Expr -> SSAPass Expr.Expr
ssaExpr vs ast = case ast of
    Expr.Accessor   id name dst -> Expr.Accessor id name <$> ssaExpr vs dst
    Expr.Var        id name     -> case (vs ^. AA.aliasMap) ^. at id of
                                        Nothing    -> Pass.fail ("Variable not found in AA!")
                                        Just alias -> case alias of
                                                      Right nid -> return $ Expr.Var id (mkVar nid)
                                                      Left  e   -> (logger error $ "Not in scope '" ++ (show e) ++ "'.")
                                                               *> (return $ Expr.Var id name)
    Expr.NativeVar  id _name     -> case (vs ^. AA.aliasMap) ^. at id of
                                        Nothing    -> Pass.fail ("Variable not found in AA!")
                                        Just alias -> case alias of
                                                      Right nid -> return $ Expr.NativeVar id (mkVar nid)
                                                      Left  e   -> Pass.fail ("Not in scope '" ++ (show e) ++ "'.")
    _                           -> continue
    where continue = Expr.traverseM (ssaExpr vs) pure ssaPat pure ast


ssaPat :: Pat -> SSAPass Pat
ssaPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> continue
    where continue = Pat.traverseM ssaPat pure pure pat
