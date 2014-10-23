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
{-# LANGUAGE TemplateHaskell           #-}

module Luna.Pass.Transform.AST.SSA.SSA where

import Control.Applicative
import Control.Monad.State

import           Flowbox.Prelude           hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import           Luna.AST.Expr             (Expr)
import qualified Luna.AST.Expr             as Expr
import           Luna.AST.Module           (Module)
import qualified Luna.AST.Module           as Module
import           Luna.AST.Pat              (Pat)
import qualified Luna.AST.Pat              as Pat
import           Luna.Data.AliasInfo       (AliasInfo)
import qualified Luna.Data.AliasInfo       as AliasInfo
import           Luna.Pass.Pass            (Pass)
import qualified Luna.Pass.Pass            as Pass



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type SSAPass result = Pass Pass.NoState result


mkVar :: Int -> String
mkVar id = "v_" ++ show id


run :: AliasInfo -> Module -> Pass.Result Module
run aliasInfo = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaModule aliasInfo)


runExpr :: AliasInfo -> Expr -> Pass.Result Expr
runExpr aliasInfo = (Pass.run_ (Pass.Info "SSA") Pass.NoState) . (ssaExpr aliasInfo)


ssaModule :: AliasInfo -> Module -> SSAPass Module
ssaModule aliasInfo mod = Module.traverseM (ssaModule aliasInfo) (ssaExpr aliasInfo) pure ssaPat pure pure mod


ssaExpr :: AliasInfo -> Expr.Expr -> SSAPass Expr.Expr
ssaExpr aliasInfo ast = case ast of
    Expr.Accessor   id name dst -> Expr.Accessor id name <$> ssaExpr aliasInfo dst
    Expr.Var        id _        -> checkVar id
    Expr.NativeVar  id _        -> checkVar id
    _                           -> continue
    where continue    = Expr.traverseM (ssaExpr aliasInfo) pure ssaPat pure pure ast
          checkVar id = case (aliasInfo ^. AliasInfo.orphans) ^. at id of
                           Just err -> (logger error $ "Not in scope '" ++ (show err) ++ "'.") *> continue
                           Nothing  -> case (aliasInfo ^. AliasInfo.alias) ^. at id of
                                           Nothing    -> Pass.fail ("Variable not found in AliasInfo!")
                                           Just nid -> return $ (ast & Expr.name .~ mkVar nid)


ssaPat :: Pat -> SSAPass Pat
ssaPat pat = case pat of
    Pat.Var  id _  -> return $ Pat.Var id (mkVar id)
    _              -> continue
    where continue = Pat.traverseM ssaPat pure pure pat
