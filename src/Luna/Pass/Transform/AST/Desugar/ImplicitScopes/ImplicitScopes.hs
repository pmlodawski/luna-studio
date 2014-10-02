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

module Luna.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes where

import           Flowbox.Prelude                                      hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.AST                                         as AST
import qualified Luna.AST.Expr                                        as Expr
import           Luna.AST.Module                                      (Module)
import qualified Luna.AST.Module                                      as Module
import           Luna.AST.Pat                                         (Pat)
import qualified Luna.AST.Pat                                         as Pat
import qualified Luna.AST.Type                                        as Type
import           Luna.Data.AliasInfo                                  (AliasInfo)
import qualified Luna.Data.AliasInfo                                  as AliasInfo
import           Luna.Data.ASTInfo                                    (ASTInfo)
import           Luna.Pass.Pass                                       (Pass)
import qualified Luna.Pass.Pass                                       as Pass
import           Luna.Pass.Transform.AST.Desugar.ImplicitScopes.State (State)
import qualified Luna.Pass.Transform.AST.Desugar.ImplicitScopes.State as State


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
        inf <- State.getAliasInfo
        let aliasMap  = inf ^. AliasInfo.alias
            parentMap = inf ^. AliasInfo.parent
            astMap    = inf ^. AliasInfo.ast
            mPid      = parentMap ^. at id
            mPAST     = (do pid <- mPid; astMap ^. at pid)
            mAlias    = aliasMap  ^. at id
            mAliasPid = (do pid <- mAlias; parentMap ^. at pid)
            mAliasAST = (do pid <- mAliasPid; astMap ^. at pid)
        case mAliasAST of
            Nothing  -> logger error ("Cannot find parent AST for variable " ++ name ++ " [" ++ show id ++ "]") *> continue
            Just ast -> case ast of
                AST.Module mod          -> Expr.Accessor id name <$> conInit
                                           where conBase = Expr.Con <$> State.genID <*> pure (mod ^. Module.cls ^. Type.name)
                                                 conInit = Expr.App <$> State.genID <*> conBase <*> pure []
                AST.Expr (Expr.Data {}) -> Expr.App <$> State.genID <*> expBase <*> pure []
                                           where expBase = Expr.Accessor id name <$> selfVar
                                                 selfVar = Expr.Var <$> State.genID <*> pure "self"
                _                       -> continue
    _                -> continue
    where continue  = Expr.traverseM desugarExpr pure desugarPat pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
