---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Luna.Data.AST.Control.Focus where

import Luna.Data.AST.Expr   (Expr)
import Luna.Data.AST.Module (Module)
import Flowbox.Prelude              hiding (Traversal)



data Focus  = Function Expr
            | Class    Expr
            | Module Module
            deriving (Show)


type FocusPath = [Focus]


type Traversal m = (Functor m, Applicative m, Monad m)



traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> Focus -> m Focus
traverseM fmod fexp focus = case focus of
    Function f -> Function <$> fexp f
    Class    c -> Class    <$> fexp c
    Module   m -> Module   <$> fmod m


traverseM_ :: Traversal m => (Module -> m r) -> (Expr -> m r) -> Focus -> m r
traverseM_ fmod fexp focus = case focus of
    Function f -> fexp f
    Class    c -> fexp c
    Module   m -> fmod m


getFunction :: Focus -> Maybe Expr
getFunction (Function expr) = Just expr
getFunction _               = Nothing


getClass :: Focus -> Maybe Expr
getClass (Class expr) = Just expr
getClass _            = Nothing


getModule :: Focus -> Maybe Module
getModule (Module m) = Just m
getModule _          = Nothing
