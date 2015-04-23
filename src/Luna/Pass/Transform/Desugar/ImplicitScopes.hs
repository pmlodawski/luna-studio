---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}


module Luna.Pass.Transform.Desugar.ImplicitScopes where

import Control.Monad (join)

import           Flowbox.Control.Monad.State hiding (State, join, mapM, mapM_)
import           Flowbox.Prelude             hiding (Traversal)
import           Luna.Data.ASTInfo           (ASTInfo)
import           Luna.Data.StructInfo        (StructInfo)
import qualified Luna.Data.StructInfo        as StructInfo
import           Luna.Pass                   (Pass (Pass), PassCtx, PassMonad)
import           Luna.Syntax.Enum            (Enumerated)
import qualified Luna.Syntax.Enum            as Enum
import qualified Luna.Syntax.Expr            as Expr
import           Luna.Syntax.Expr            (LExpr)
import           Luna.Syntax.Label           (Label (Label))
import qualified Luna.Syntax.Traversals      as AST

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data PassState = PassState { _astInfo    :: ASTInfo
                           , _structInfo :: StructInfo
                           } deriving (Show)


makeLenses ''PassState

data ImplScopes = ImplScopes

type ISPass                 m     = PassMonad PassState m
type ISCtx              lab m a   = (Monad m, Enumerated lab, ISTraversal m a, Num lab)
type ISTraversal            m a   = (PassCtx m, AST.Traversal        ImplScopes (ISPass m) a a)
type ISDefaultTraversal     m a   = (PassCtx m, AST.DefaultTraversal ImplScopes (ISPass m) a a)


------------------------------------------------------------------------
---- Utils functions
------------------------------------------------------------------------

traverseM :: (ISTraversal m a) => a -> ISPass m a
traverseM = AST.traverseM ImplScopes

defaultTraverseM :: (ISDefaultTraversal m a) => a -> ISPass m a
defaultTraverseM = AST.defaultTraverseM ImplScopes


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: ISDefaultTraversal m a => Pass PassState (ASTInfo -> StructInfo -> a -> ISPass m (a, ASTInfo))
pass = Pass "Implicit self" "Desugars AST by adding implicit self function parameters" undefined passRunner
-- TODO [wd]: ^----------------^-- add more detailed description, so the difference b/w this and ImplicitSelf would be clear

passRunner ai si ast = do
    put $ PassState ai si
    (,) <$> defaultTraverseM ast <*> (view astInfo <$> get)

exprScopes :: ISCtx lab m a => LExpr lab a -> ISPass m (LExpr lab a)
exprScopes ast@(Label lab e) = case e of
    Expr.Var (Expr.Variable name v) -> fmake ast id
    --Expr.Curry v@(Label lab' (Expr.Var {}) ->  fmake v
    _                               -> continue
    where continue = defaultTraverseM ast

fmake ast@(Label lab e) f = case e of
    Expr.Var (Expr.Variable name v) -> do
        si <- view structInfo <$> get
        let parentMap = view StructInfo.parent si
            aliasMap  = view StructInfo.alias si
            pid       = view (at id) parentMap
            tgt       = fmap (view StructInfo.target) $ view (at id) aliasMap
            tgtPid    = join $ fmap (\tid -> view (at tid) parentMap) tgt

        if pid == tgtPid then return ast
                         else return $ Label (-888) $ Expr.Accessor (convert name) (f $ Label lab $ Expr.Var $ Expr.Variable "self" v)
                         -- TODO [kgdk -> wd]: ^-- a magic constant :)
    where id = Enum.id lab
----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance ISCtx lab m a => AST.Traversal ImplScopes (ISPass m) (LExpr lab a) (LExpr lab a) where
    traverseM _ = exprScopes

