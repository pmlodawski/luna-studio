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

module Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.ImplicitScopes where

import           Flowbox.Prelude                                          hiding (error, id, mod)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.AST                                         as AST
import qualified Luna.DEP.AST.Expr                                        as Expr
import           Luna.DEP.AST.Module                                      (Module)
import qualified Luna.DEP.AST.Module                                      as Module
import qualified Luna.DEP.AST.Name                                        as Name
import           Luna.DEP.AST.Pat                                         (Pat)
import qualified Luna.DEP.AST.Pat                                         as Pat
import qualified Luna.DEP.AST.Type                                        as Type
import           Luna.DEP.Data.AliasInfo                                  (AliasInfo)
import qualified Luna.DEP.Data.AliasInfo                                  as AliasInfo
import           Luna.DEP.Data.ASTInfo                                    (ASTInfo)
import           Luna.DEP.Pass.Pass                                       (Pass)
import qualified Luna.DEP.Pass.Pass                                       as Pass
import           Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.State (State)
import qualified Luna.DEP.Pass.Transform.AST.Desugar.ImplicitScopes.State as State


logger :: LoggerIO
logger = getLoggerIO $moduleName


type DesugarPass result = Pass State result


run :: ASTInfo -> AliasInfo -> Module -> Pass.Result (Module, ASTInfo)
run inf va = (Pass.run_ (Pass.Info "Desugar.ImplicitScopes") $ State.mk inf va) . desugar


desugar :: Module -> DesugarPass (Module, ASTInfo)
desugar mod = (,) <$> desugarModule mod <*> State.getAstInfo


desugarModule :: Module -> DesugarPass Module
desugarModule mod = Module.traverseM desugarModule desugarExpr pure desugarPat pure pure mod


desugarExpr :: Expr.Expr -> DesugarPass Expr.Expr
desugarExpr ast = case ast of
    Expr.FuncVar id name -> desugarExpr $ Expr.Var id $ view Name.base name
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
            nameAcc   = Expr.VarAccessor name
        case mAliasAST of
            Nothing  -> logger error ("Cannot find parent AST for variable " ++ name ++ " [" ++ show id ++ "]") *> continue
            Just ast -> case ast of
                AST.Module mod          -> Expr.Accessor id nameAcc <$> conInit
                                           where conBase = Expr.Con <$> State.genID <*> pure (mod ^. Module.cls ^. Type.name)
                                                 conInit = Expr.App <$> State.genID <*> conBase <*> pure []
                AST.Expr (Expr.Data {}) -> Expr.App <$> State.genID <*> expBase <*> pure []
                                           where expBase = Expr.Accessor id nameAcc <$> selfVar
                                                 selfVar = Expr.Var <$> State.genID <*> pure "self"
                _                       -> continue
    _                -> continue
    where continue  = Expr.traverseM desugarExpr pure desugarPat pure pure ast


desugarPat :: Pat -> DesugarPass Pat
desugarPat pat = case pat of
    _              -> continue
    where continue = Pat.traverseM desugarPat pure pure pat
