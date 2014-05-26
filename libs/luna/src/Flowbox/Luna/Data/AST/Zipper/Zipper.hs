---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Flowbox.Luna.Data.AST.Zipper.Zipper where

import           Data.List                               (find, filter)
import           Flowbox.Control.Error                   (assert)
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Crumb.Crumb       (Crumb)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb       as Crumb
import qualified Flowbox.Luna.Data.AST.Expr              as Expr
import           Flowbox.Luna.Data.AST.Module            (Module)
import qualified Flowbox.Luna.Data.AST.Module            as Module
import qualified Flowbox.Luna.Data.AST.Type              as Type
import           Flowbox.Luna.Data.AST.Zipper.Focus      (Focus (ModuleFocus, ClassFocus, FunctionFocus), FocusPath)
import           Flowbox.Prelude                         hiding (drop, id, mod, Zipper)


type Zipper = (Focus, FocusPath)


mk :: (Applicative m, Monad m) => Module -> m Zipper
mk rootmod = return (ModuleFocus rootmod, [])


defocus :: Zipper -> Zipper
defocus zipper@(_, [])     = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        ModuleFocus pmod -> ModuleFocus $ case env of
            FunctionFocus fun -> Module.addMethod fun pmod
            ClassFocus    cls -> Module.addClass  cls pmod
            ModuleFocus   mod -> Module.addModule mod pmod
        ClassFocus  pcls -> ClassFocus $ case env of
            FunctionFocus fun -> Expr.addMethod fun pcls
            ClassFocus    cls -> Expr.addClass  cls pcls


defocusDrop :: Zipper -> Zipper
defocusDrop zipper@(_, [])   = zipper
defocusDrop (_, parent:path) = (parent, path)


modify :: (Applicative m, Monad m) => (Focus -> Focus) -> Zipper -> m Zipper
modify f (env, path) = return (f env, path)


close :: (Applicative m, Monad m) => Zipper -> m Module
close (env, []) = return mod where ModuleFocus mod = env
close zipper    = close $ defocus zipper


focusClass :: (Applicative m, Monad m) => String -> Zipper -> m Zipper
focusClass name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.classes classComp
                       ClassFocus ModuleFocus mod zipper
    ClassFocus  cls -> focusListElem Expr.classes classComp
                       ClassFocus ClassFocus  cls zipper
    _               -> fail $ "Cannot focus on " ++ (show name)
    where classComp f = f ^. (Expr.cls . Type.name) == name


focusFunction :: (Applicative m, Monad m) => String -> [String] -> Zipper -> m Zipper
focusFunction name path zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.methods funComp
                       FunctionFocus ModuleFocus mod zipper
    ClassFocus  cls -> focusListElem Expr.methods funComp
                       FunctionFocus ClassFocus cls zipper
    _               -> fail $ "Cannot focus on " ++ (show name)
    where funComp f = f ^. Expr.name == name && f ^. Expr.path == path


focusModule :: (Applicative m, Monad m) => String -> Zipper -> m Zipper
focusModule name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.modules modComp
                       ModuleFocus ModuleFocus mod zipper
    _               -> fail $ "Cannot focus on " ++ (show name)
    where modComp f = f ^. (Module.cls . Type.name) == name


focusCrumb :: (Applicative m, Monad m) => Crumb -> Zipper -> m Zipper
focusCrumb c = case c of
    Crumb.ClassCrumb    name      -> focusClass    name
    Crumb.FunctionCrumb name path -> focusFunction name path
    Crumb.ModuleCrumb   name      -> focusModule   name


focusBreadcrumbs :: (Applicative m, Monad m) => Breadcrumbs -> Zipper -> m Zipper
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> focusCrumb h zipper >>= focusBreadcrumbs t


focusModule' :: (Applicative m, Monad m) => String -> Module -> m Zipper
focusModule' name m = do
    assert (m ^. Module.cls . Type.name == name) $ "Cannot focus on " ++ (show name)
    mk m


focusCrumb' :: (Applicative m, Monad m) => Crumb -> Module -> m Zipper
focusCrumb' c m = case c of
    Crumb.ModuleCrumb name -> focusModule' name m
    _                      -> fail $ "Cannot focus on " ++ (show c)


focusBreadcrumbs' :: (Applicative m, Monad m) => Breadcrumbs -> Module -> m Zipper
focusBreadcrumbs' bc m = case bc of
    h:t -> focusCrumb' h m >>= focusBreadcrumbs t
    _   -> fail $ "Cannot focus on " ++ (show bc)


getFocus :: Zipper -> Focus
getFocus = fst

--f = (\f -> f ^. (Expr.cls . Type.name) == name)
--focusListElem Module.classes (Expr.cls . Type.name)  ClassFocus ModuleFocus mod name zipper


focusListElem :: (Applicative m, Monad m) => Traversal' a [b] -> (b -> Bool) -> (b -> Focus) -> (a -> Focus) -> a -> Zipper -> m Zipper
focusListElem flens match elemFocus crumbFocus el (_, path) = do
    let funcs    = el ^. flens
        mfunc    = find match funcs
        newfuncs = filter (\a -> not $ match a) funcs
        newelem  = el & flens .~ newfuncs
    func <- case mfunc of
                Just func -> return func
                Nothing   -> fail $ "Cannot find element in AST."
    return $ (elemFocus func, (crumbFocus newelem) : path)
