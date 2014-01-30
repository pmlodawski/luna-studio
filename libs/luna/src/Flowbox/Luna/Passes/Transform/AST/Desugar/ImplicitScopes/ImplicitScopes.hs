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

module Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes where

import qualified Flowbox.Luna.Data.AST.Expr                                     as Expr
import           Flowbox.Luna.Data.AST.Module                                   (Module)
import qualified Flowbox.Luna.Data.AST.Module                                   as Module
import           Flowbox.Luna.Data.AST.Pat                                      (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                                      as Pat
import qualified Flowbox.Luna.Data.AST.Type                                     as Type
import qualified Flowbox.Luna.Data.AST.Module                                   as Module
import qualified Flowbox.Luna.Data.AST.AST                                      as AST
import           Flowbox.Luna.Data.Pass.AliasInfo                               (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo as AliasInfo
import           Flowbox.Luna.Data.Pass.ASTInfo                                 (ASTInfo)
import           Flowbox.Luna.Passes.Pass                                       (Pass)
import qualified Flowbox.Luna.Passes.Pass                                       as Pass
import           Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.State (State)
import qualified Flowbox.Luna.Passes.Transform.AST.Desugar.ImplicitScopes.State as State
import           Flowbox.Prelude                                                hiding (error, id, mod)
import           Flowbox.System.Log.Logger


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.AST.Desugar.ImplicitScopes.ImplicitScopes"


type DesugarPass result = Pass State result


run :: ASTInfo -> AliasInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf va = (Pass.run_ (Pass.Info "Desugar.ImplicitScopes") $ State.mk inf va) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> State.getAstInfo


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    Expr.Var id name -> do
                        info <- State.getAliasInfo
                        let aliasMap  = info ^. AliasInfo.aliasMap
                            parentMap = info ^. AliasInfo.parentMap
                            astMap    = info ^. AliasInfo.astMap
                            mAlias    = aliasMap  ^. at id
                            mPid      = parentMap ^. at id
                            mAliasPid = (do pid <- mAlias; parentMap ^. at pid)
                            mAliasAST = (do pid <- mAliasPid; astMap ^. at pid)
                        case mAliasAST of
                            Nothing  -> continue
                            Just ast -> case ast of
                                AST.Module mod -> Expr.Accessor id name <$> conInit
                                                  where conBase = Expr.Con <$> State.genID <*> pure (mod ^. Module.cls ^. Type.name)
                                                        conInit = Expr.App <$> State.genID <*> conBase <*> pure []
                                _              -> continue 
    _                -> continue
    where continue  = Expr.traverseM desugarExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
