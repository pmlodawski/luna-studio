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
import           Flowbox.Control.Monad.Trans.Maybe
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


mk :: Module -> Maybe Zipper
mk rootmod = Just (ModuleFocus rootmod, [])


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


modify :: (Focus -> Focus) -> Zipper -> Maybe Zipper
modify f (env, path) = Just (f env, path)


close :: Zipper -> Maybe Module
close (env, []) = Just mod where ModuleFocus mod = env
close zipper    = close $ defocus zipper


focusClass :: String -> Zipper -> Maybe Zipper
focusClass name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.classes (Expr.cls . Type.name)
                       ClassFocus ModuleFocus mod name zipper
    ClassFocus  cls -> focusListElem Expr.classes (Expr.cls . Type.name)
                       ClassFocus ClassFocus  cls name zipper
    _               -> Nothing


focusFunction :: String -> Zipper -> Maybe Zipper
focusFunction name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.methods Expr.name
                       FunctionFocus ModuleFocus mod name zipper
    ClassFocus  cls -> focusListElem Expr.methods Expr.name
                       FunctionFocus ClassFocus  cls name zipper
    _               -> Nothing


focusModule :: String -> Zipper -> Maybe Zipper
focusModule name zipper@(env, _) = case env of
    ModuleFocus mod -> focusListElem Module.modules (Module.cls . Type.path . (to last))
                       ModuleFocus ModuleFocus mod name zipper
    _               -> Nothing



focusBreadcrumbs :: Breadcrumbs -> Zipper -> Maybe Zipper
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> let f = case h of
                       Crumb.ClassCrumb    name -> focusClass    name
                       Crumb.FunctionCrumb name -> focusFunction name
                       Crumb.ModuleCrumb   name -> focusModule   name
           in f zipper >>= focusBreadcrumbs t 


getFocus :: Zipper -> Focus
getFocus = fst


focusListElem :: Traversal' a [b] -> Fold b String -> (b -> Focus) -> (a -> Focus) -> a -> String -> Zipper -> Maybe Zipper
focusListElem flens nameLens elemFocus crumbFocus el name (_, path) = runMaybe $ do
    let funcs    = el ^. flens
        mfunc    = find (\f -> f ^. nameLens == name) funcs
        newfuncs = [ f | f <- funcs, f ^. nameLens /= name ]
        newelem  = el & flens .~ newfuncs
    func <- hoistMaybe mfunc
    return $ (elemFocus func, (crumbFocus newelem) : path)
