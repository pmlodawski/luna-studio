---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Data.AST.Zipper where

import           Data.List                         (find)
import           Flowbox.Luna.Data.AST.Crumb.Crumb (Breadcrumbs)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb as Crumb
import           Flowbox.Luna.Data.AST.Expr        (Expr)
import qualified Flowbox.Luna.Data.AST.Expr        as Expr
import           Flowbox.Luna.Data.AST.Module      (Module)
import qualified Flowbox.Luna.Data.AST.Module      as Module
import qualified Flowbox.Luna.Data.AST.Type        as Type
import           Flowbox.Prelude                   hiding (Zipper, drop, id, mod, zipper)

--data Focus = FunctionFocus { expr :: Expr   , env :: FocusEnv }
--           | ClassFocus    { expr :: Expr   , env :: FocusEnv }
--           | ModuleFocus   { mod  :: Module , env :: FocusEnv }
--           deriving (Show)

data Focus  = FunctionFocus Expr
            | ClassFocus    Expr
            | ModuleFocus Module
            deriving (Show)


type FocusPath = [Focus]


type Zipper = (Focus, FocusPath)


mk :: (Applicative m, Monad m) => Module -> m Zipper
mk rootmod = return (ModuleFocus rootmod, [])


defocus :: Zipper -> Zipper
defocus zipper@(_, [])   = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        ModuleFocus pmod -> ModuleFocus $ case env of
            FunctionFocus fun -> Module.addMethod fun pmod
            ClassFocus    cls -> Module.addClass  cls pmod
            ModuleFocus   mod -> Module.addModule mod pmod
        ClassFocus  pcls -> ClassFocus $ case env of
            FunctionFocus fun -> Expr.addMethod fun pcls
            ClassFocus    cls -> Expr.addClass  cls pcls


modify :: (Applicative m, Monad m) => (Focus -> Focus) -> Zipper -> m Zipper
modify f (env, path) = return (f env, path)


close :: (Applicative m, Monad m) => Zipper -> m Module
close (env, []) = return mod where ModuleFocus mod = env
close zipper    = close $ defocus zipper


focusClass :: (Applicative m, Monad m) => String -> Zipper -> m Zipper
focusClass name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.classes (Expr.cls . Type.name)
                       ClassFocus ModuleFocus mod name zipper
    ClassFocus  cls -> focusListElem Expr.classes (Expr.cls . Type.name)
                       ClassFocus ClassFocus  cls name zipper
    _               -> fail $ "Cannot focus on " ++ (show name)


focusFunction :: (Applicative m, Monad m) => String -> Zipper -> m Zipper
focusFunction name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.methods Expr.name
                       FunctionFocus ModuleFocus mod name zipper
    ClassFocus  cls -> focusListElem Expr.methods Expr.name
                       FunctionFocus ClassFocus  cls name zipper
    _               -> fail $ "Cannot focus on " ++ (show name)


focusModule :: (Applicative m, Monad m) => String -> Zipper -> m Zipper
focusModule name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.modules (Module.cls . Type.path . (to last))
                       ModuleFocus ModuleFocus mod name zipper
    _               -> fail $ "Cannot focus on " ++ (show name)


focusBreadcrumbs :: (Applicative m, Monad m) => Breadcrumbs -> Zipper -> m Zipper
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> let f = case h of
                       Crumb.ClassCrumb    name -> focusClass    name
                       Crumb.FunctionCrumb name -> focusFunction name
                       Crumb.ModuleCrumb   name -> focusModule   name
           in f zipper >>= focusBreadcrumbs t


getFocus :: Zipper -> Focus
getFocus = fst


focusListElem :: (Applicative m, Monad m) => Traversal' a [b] -> Fold b String -> (b -> Focus) -> (a -> Focus) -> a -> String -> Zipper -> m Zipper
focusListElem flens nameLens elemFocus crumbFocus el name (_, path) = do
    let funcs    = el ^. flens
        mfunc    = find (\f -> f ^. nameLens == name) funcs
        newfuncs = [ f | f <- funcs, f ^. nameLens /= name ]
        newelem  = el & flens .~ newfuncs
    func <- case mfunc of
                Just func -> return func
                Nothing   -> fail $ "Cannot find " ++ (show name) ++ " in AST."
    return $ (elemFocus func, (crumbFocus newelem) : path)
