---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Luna.Control.Focus where

import Control.Zipper ((:>>), Top)

import Flowbox.Prelude
import Luna.Syntax.Decl   (LDecl)
import Luna.Syntax.Module (LModule)
--import Luna.Syntax.Expr   (Expr)
--import           Luna.Syntax.Arg    (Arg)
--import qualified Luna.Syntax.Expr   as Expr
--import           Luna.Syntax.Lit    (Lit)
--import qualified Luna.Syntax.Module as Module
--import           Luna.Syntax.Pat    (Pat)
--import           Luna.Syntax.Type   (Type)


data Focus a v = Lambda   { _decl :: LDecl   a v }
               | Function { _decl :: LDecl   a v }
               | Data     { _decl :: LDecl   a v }
               | Module   { _module_ :: LModule a v }
               deriving (Show)

data FocusZ a v = ModuleZ { _modZipper :: Top :>> LModule a v :>> LDecl a v }
                | DataZ   { _dclZipper :: Top :>> LDecl   a v :>> LDecl a v }

makeLenses ''Focus
makeLenses ''FocusZ

type FocusPath a v = [ FocusZ a v]
--type FocusPath a v = [Zipper (Zipper Top Int (LDecl a v)) Int (LDecl a v)]
--type FocusPath a v = [Top :>> LDecl a v :>> LDecl a v]


--type Traversal m = (Functor m, Applicative m, Monad m)

--traverseM :: Traversal m => (Module -> m Module) -> (Expr -> m Expr) -> Focus -> m Focus
--traverseM fmod fexp focus = case focus of
--    Lambda   l -> Lambda   <$> fexp l
--    Function f -> Function <$> fexp f
--    Data     c -> Data     <$> fexp c
--    Module   m -> Module   <$> fmod m


--traverseM_ :: Traversal m => (Module -> m r) -> (Expr -> m r) -> Focus -> m r
--traverseM_ fmod fexp focus = case focus of
--    Lambda   l -> fexp l
--    Function f -> fexp f
--    Data     c -> fexp c
--    Module   m -> fmod m


--traverseMR :: Traversal m => (Module -> m Module) -> (Expr -> m Expr)
--           -> (Type -> m Type) -> (Pat -> m Pat) -> (Lit -> m Lit) -> (Arg Expr -> m (Arg Expr))
--           -> Focus -> m Focus
--traverseMR fmod fexp ftype fpat flit farg focus = case focus of
--    Lambda   expr  -> Lambda   <$> Expr.traverseMR fexp ftype fpat flit farg expr
--    Function expr  -> Function <$> Expr.traverseMR fexp ftype fpat flit farg expr
--    Data     expr  -> Data     <$> Expr.traverseMR fexp ftype fpat flit farg expr
--    Module module_ -> Module   <$> Module.traverseMR fmod fexp ftype fpat flit farg module_


getLambda :: Focus a v -> Maybe (LDecl a v)
getLambda (Lambda l) = Just l
getLambda _          = Nothing


getFunction :: Focus a v -> Maybe (LDecl a v)
getFunction (Function e) = Just e
getFunction _            = Nothing


getData :: Focus a v -> Maybe (LDecl a v)
getData (Data e) = Just e
getData _        = Nothing


getModule :: Focus a v -> Maybe (LModule a v)
getModule (Module m) = Just m
getModule _          = Nothing

