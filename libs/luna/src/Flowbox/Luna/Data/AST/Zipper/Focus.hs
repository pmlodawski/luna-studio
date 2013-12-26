---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Flowbox.Luna.Data.AST.Zipper.Focus where

import           Flowbox.Luna.Data.AST.Expr   (Expr)
import qualified Flowbox.Luna.Data.AST.Expr   as Expr
import           Flowbox.Luna.Data.AST.Lit    (Lit)
import           Flowbox.Luna.Data.AST.Module (Module)
import qualified Flowbox.Luna.Data.AST.Module as Module
import           Flowbox.Luna.Data.AST.Pat    (Pat)
import           Flowbox.Luna.Data.AST.Type   (Type)
import           Flowbox.Prelude              hiding (Traversal, focus)



data Focus  = FunctionFocus Expr
            | ClassFocus    Expr
            | ModuleFocus Module
            deriving (Show)


type FocusPath = [Focus]


type Traversal m = (Functor m, Applicative m, Monad m)



traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> Focus -> m Focus
traverseM fmod fexp ftype fpat flit focus = case focus of
    FunctionFocus f -> FunctionFocus <$> Expr.traverseM fexp ftype fpat flit f
    ClassFocus    c -> ClassFocus    <$> Expr.traverseM fexp ftype fpat flit c
    ModuleFocus   m -> ModuleFocus   <$> Module.traverseM fmod fexp ftype fpat flit m


traverseM_ :: Traversal m => (Module -> m r) -> (Expr -> m r) -> (Type -> m r) -> (Pat -> m r) -> (Lit -> m r) -> Focus -> m ()
traverseM_ fmod fexp ftype fpat flit focus = case focus of
    FunctionFocus f -> Expr.traverseM_ fexp ftype fpat flit f
    ClassFocus    c -> Expr.traverseM_ fexp ftype fpat flit c
    ModuleFocus   m -> Module.traverseM_ fmod fexp ftype fpat flit m
