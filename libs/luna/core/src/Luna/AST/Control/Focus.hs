---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}

module Luna.AST.Control.Focus where

import Flowbox.Prelude hiding (Traversal)
import Luna.AST.Expr   (Expr)
import Luna.AST.Module (Module)



data Focus  = Lambda   Expr
            | Function Expr
            | Class    Expr
            | Module Module
            deriving (Show)


type FocusPath = [Focus]


type Traversal m = (Functor m, Applicative m, Monad m)



traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> Focus -> m Focus
traverseM fmod fexp focus = case focus of
    Lambda   l -> Lambda   <$> fexp l
    Function f -> Function <$> fexp f
    Class    c -> Class    <$> fexp c
    Module   m -> Module   <$> fmod m


traverseM_ :: Traversal m => (Module -> m r) -> (Expr -> m r) -> Focus -> m r
traverseM_ fmod fexp focus = case focus of
    Lambda   l -> fexp l
    Function f -> fexp f
    Class    c -> fexp c
    Module   m -> fmod m


getLambda :: Focus -> Maybe Expr
getLambda (Lambda l) = Just l
getLambda _          = Nothing


getFunction :: Focus -> Maybe Expr
getFunction (Function expr) = Just expr
getFunction _               = Nothing


getClass :: Focus -> Maybe Expr
getClass (Class expr) = Just expr
getClass _            = Nothing


getModule :: Focus -> Maybe Module
getModule (Module m) = Just m
getModule _          = Nothing
