---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Luna.AST.Control.Zipper where

import           Data.List                               (find)
import           Flowbox.Control.Error                   (assert)
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Crumb.Crumb       (Crumb)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb       as Crumb
import qualified Flowbox.Luna.Data.AST.Expr              as Expr
import           Flowbox.Luna.Data.AST.Module            (Module)
import qualified Flowbox.Luna.Data.AST.Module            as Module
import qualified Flowbox.Luna.Data.AST.Type              as Type
import           Flowbox.Luna.Data.AST.Zipper.Focus      (Focus, FocusPath)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus      as Focus
import           Flowbox.Prelude                         hiding (drop, id, mod)



type Zipper = (Focus, FocusPath)

type Error = String


mk :: Module -> Zipper
mk rootmod = (Focus.Module rootmod, [])


defocus :: Zipper -> Zipper
defocus zipper@(_, [])     = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        Focus.Module pmod -> Focus.Module $ case env of
            Focus.Function fun -> Module.addMethod fun pmod
            Focus.Class    cls -> Module.addClass  cls pmod
            Focus.Module   mod -> Module.addModule mod pmod
        Focus.Class  pcls -> Focus.Class $ case env of
            Focus.Function fun -> Expr.addMethod fun pcls
            Focus.Class    cls -> Expr.addClass  cls pcls


defocusDrop :: Zipper -> Zipper
defocusDrop zipper@(_, [])   = zipper
defocusDrop (_, parent:path) = (parent, path)


modify :: (Focus -> Focus) -> Zipper -> Zipper
modify f (env, path) = (f env, path)


close :: Zipper -> Module
close (env, []) = mod where Focus.Module mod = env
close zipper    = close $ defocus zipper


focusClass :: String -> Zipper -> Either Error Zipper
focusClass name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.classes classComp
                       Focus.Class Focus.Module mod zipper
    Focus.Class  cls -> focusListElem Expr.classes classComp
                       Focus.Class Focus.Class  cls zipper
    _               -> Left $ "Cannot focus on " ++ show name
    where classComp f = f ^. (Expr.cls . Type.name) == name


focusFunction :: String -> [String] -> Zipper -> Either Error Zipper
focusFunction name path zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.methods funComp
                       Focus.Function Focus.Module mod zipper
    Focus.Class  cls -> focusListElem Expr.methods funComp
                       Focus.Function Focus.Class cls zipper
    _               -> Left $ "Cannot focus on " ++ show name
    where funComp f = f ^. Expr.name == name && f ^. Expr.path == path


focusModule :: String -> Zipper -> Either Error Zipper
focusModule name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.modules modComp
                       Focus.Module Focus.Module mod zipper
    _               -> Left $ "Cannot focus on " ++ show name
    where modComp f = f ^. (Module.cls . Type.name) == name


focusCrumb :: Crumb -> Zipper -> Either Error Zipper
focusCrumb c = case c of
    Crumb.Class    name      -> focusClass    name
    Crumb.Function name path -> focusFunction name path
    Crumb.Module   name      -> focusModule   name


focusBreadcrumbs :: Breadcrumbs -> Zipper -> Either Error Zipper
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> focusCrumb h zipper >>= focusBreadcrumbs t


focusModule' :: String -> Module -> Either Error Zipper
focusModule' name m = do
    assert (m ^. Module.cls . Type.name == name) $ "Cannot focus on " ++ show name
    return $ mk m


focusCrumb' :: Crumb -> Module -> Either Error Zipper
focusCrumb' c m = case c of
    Crumb.Module name -> focusModule' name m
    _                 -> Left $ "Cannot focus on " ++ show c


focusBreadcrumbs' :: Breadcrumbs -> Module -> Either Error Zipper
focusBreadcrumbs' bc m = case bc of
    h:t -> focusCrumb' h m >>= focusBreadcrumbs t
    _   -> Left $ "Cannot focus on " ++ show bc


getFocus :: Zipper -> Focus
getFocus = fst


focusListElem :: Traversal' a [b] -> (b -> Bool) -> (b -> Focus) -> (a -> Focus) -> a -> Zipper -> Either Error Zipper
focusListElem flens match elemFocus crumbFocus el (_, path) = do
    let funcs    = el ^. flens
        mfunc    = find match funcs
        newfuncs = filter (not . match) funcs
        newelem  = el & flens .~ newfuncs
    func <- case mfunc of
                Just func -> return func
                Nothing   -> Left "Cannot find element in AST."
    return (elemFocus func, crumbFocus newelem : path)
