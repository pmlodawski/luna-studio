---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.AST.Control.Focus where

import           Flowbox.Prelude     hiding (Traversal)
import           Luna.DEP.AST.Arg    (Arg)
import           Luna.DEP.AST.Expr   (Expr)
import qualified Luna.DEP.AST.Expr   as Expr
import           Luna.DEP.AST.Lit    (Lit)
import           Luna.DEP.AST.Module (Module)
import qualified Luna.DEP.AST.Module as Module
import           Luna.DEP.AST.Pat    (Pat)
import           Luna.DEP.AST.Type   (Type)



data Focus  = Lambda   { _expr :: Expr   }
            | Function { _expr :: Expr   }
            | Class    { _expr :: Expr   }
            | Module   { _module_ :: Module }
            deriving (Show)

makeLenses ''Focus


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


traverseMR :: Traversal m => (Module -> m Module) -> (Expr -> m Expr)
           -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> (Arg Expr -> m (Arg Expr))
           -> Focus -> m Focus
traverseMR fmod fexp ftype fpat flit farg focus = case focus of
    Lambda   expr  -> Lambda   <$> Expr.traverseMR fexp ftype fpat flit farg expr
    Function expr  -> Function <$> Expr.traverseMR fexp ftype fpat flit farg expr
    Class    expr  -> Class    <$> Expr.traverseMR fexp ftype fpat flit farg expr
    Module module_ -> Module   <$> Module.traverseMR fmod fexp ftype fpat flit farg module_


getLambda :: Focus -> Maybe Expr
getLambda (Lambda l) = Just l
getLambda _          = Nothing


getFunction :: Focus -> Maybe Expr
getFunction (Function e) = Just e
getFunction _            = Nothing


getClass :: Focus -> Maybe Expr
getClass (Class e) = Just e
getClass _         = Nothing


getModule :: Focus -> Maybe Module
getModule (Module m) = Just m
getModule _          = Nothing

