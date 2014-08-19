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

module Luna.Pass.Transform.AST.SSA.SSA where

import Control.Applicative
import Control.Monad.State

import qualified Flowbox.Luna.Data.AST.Expr       as Expr
import           Flowbox.Luna.Data.AST.Module     (Module)
import qualified Flowbox.Luna.Data.AST.Module     as Module
import           Flowbox.Luna.Data.AST.Pat        (Pat)
import qualified Flowbox.Luna.Data.AST.Pat        as Pat
import           Flowbox.Luna.Data.Pass.AliasInfo (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo as AliasInfo
import           Flowbox.Luna.Passes.Pass         (Pass)
import qualified Flowbox.Luna.Passes.Pass         as Pass
import           Flowbox.Prelude                  hiding (error, id, mod)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.SSA.SSA"


type SSAPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: AliasInfo -> Module -> Pass.Result Module
run aliasInfo = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaModule aliasInfo)


ssaModule :: AliasInfo -> Module -> SSAPass Module
ssaModule aliasInfo mod = Module.traverseM (ssaModule aliasInfo) (ssaExpr aliasInfo) pure ssaPat pure mod


ssaExpr :: AliasInfo -> Expr.Expr -> SSAPass Expr.Expr
ssaExpr aliasInfo ast = case ast of
    Expr.Accessor   id name dst -> Expr.Accessor id name <$> ssaExpr aliasInfo dst
    Expr.Var        id _        -> checkVar id
    Expr.NativeVar  id _        -> checkVar id
    _                           -> continue
    where continue    = Expr.traverseM (ssaExpr aliasInfo) pure ssaPat pure ast
          checkVar id = case (aliasInfo ^. AliasInfo.invalidMap) ^. at id of
                           Just err -> (logger error $ "Not in scope '" ++ (show err) ++ "'.") *> continue
                           Nothing  -> case (aliasInfo ^. AliasInfo.aliasMap) ^. at id of
                                           Nothing    -> Pass.fail ("Variable not found in AliasInfo!")
                                           Just nid -> return $ (ast & Expr.name .~ mkVar nid)


ssaPat :: Pat -> SSAPass Pat
ssaPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> continue
    where continue = Pat.traverseM ssaPat pure pure pat
