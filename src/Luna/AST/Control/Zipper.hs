---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Luna.AST.Control.Zipper where

import Control.Monad.Trans.State
import Data.List                 (find)

import           Flowbox.Control.Error  (assert, (<?>))
import           Flowbox.Prelude        hiding (drop, id, mod)
import qualified Luna.AST.Common        as AST
import           Luna.AST.Control.Crumb (Breadcrumbs, Crumb)
import qualified Luna.AST.Control.Crumb as Crumb
import           Luna.AST.Control.Focus (Focus, FocusPath)
import qualified Luna.AST.Control.Focus as Focus
import           Luna.AST.Expr          (Expr)
import qualified Luna.AST.Expr          as Expr
import           Luna.AST.Module        (Module)
import qualified Luna.AST.Module        as Module
import           Luna.AST.Name          (Name)
import qualified Luna.AST.Type          as Type



type Zipper = (Focus, FocusPath)

type Error = String


mk :: Module -> Zipper
mk rootmod = (Focus.Module rootmod, [])


defocus :: Zipper -> Zipper
defocus zipper@(_, [])     = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        Focus.Module   pmod -> Focus.Module $ case env of
            Focus.Lambda   lam -> defocusModuleL pmod lam
            Focus.Function fun -> Module.addMethod fun pmod
            Focus.Class    cls -> Module.addClass  cls pmod
            Focus.Module   mod -> Module.addModule mod pmod
        Focus.Class    pcls -> Focus.Class $ case env of
            Focus.Function fun -> Expr.addMethod fun pcls
            Focus.Class    cls -> Expr.addClass  cls pcls
            _                  -> error "Zipper.defocus"
        Focus.Function pfun -> Focus.Function $ case env of
            Focus.Lambda   lam -> defocusExprL pfun lam
            _                  -> error "Zipper.defocus"
        Focus.Lambda   plam -> Focus.Lambda $ case env of
            Focus.Lambda   lam -> defocusExprL plam lam
            _                  -> error "Zipper.defocus"

    defocusModuleL par lam = case replaceModuleByAstID par lam of
        Nothing        -> error "Zipper.defocus"
        Just (par', _) -> par'
    defocusExprL par lam   = case replaceExprByAstID par lam of
        Nothing        -> error "Zipper.defocus"
        Just (par', _) -> par'


defocusDrop :: Zipper -> Zipper
defocusDrop zipper@(_, [])   = zipper
defocusDrop (_, parent:path) = (parent, path)


modify :: (Focus -> Focus) -> Zipper -> Zipper
modify f (env, path) = (f env, path)


close :: Zipper -> Module
close (env, []) = mod where Focus.Module mod = env
close zipper    = close $ defocus zipper


focusModule :: String -> Zipper -> Either Error Zipper
focusModule name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.modules modComp
                            Focus.Module Focus.Module mod zipper
    _                -> Left $ "Zipper.focusModule : Cannot focus on " ++ show name
    where modComp f = f ^. (Module.cls . Type.name) == name


focusClass :: String -> Zipper -> Either Error Zipper
focusClass name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.classes classComp
                            Focus.Class Focus.Module mod zipper
    Focus.Class  cls -> focusListElem Expr.classes classComp
                            Focus.Class Focus.Class  cls zipper
    _                -> Left $ "Zipper.focusClass : Cannot focus on " ++ show name
    where classComp f = f ^. (Expr.cls . Type.name) == name


focusFunction :: Name -> [String] -> Zipper -> Either Error Zipper
focusFunction name path zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem Module.methods funComp
                            Focus.Function Focus.Module mod zipper
    Focus.Class  cls -> focusListElem Expr.methods funComp
                            Focus.Function Focus.Class cls zipper
    _                -> Left $ "Zipper.focusFunction : Cannot focus on " ++ show name
    where funComp f = (f ^?! Expr.fname) == name && f ^. Expr.path == path


focusLambda :: AST.ID -> Zipper -> Either Error Zipper
focusLambda astID (env, path) = case env of
    Focus.Module   mod -> focusModuleL mod Focus.Module
    Focus.Class    cls -> focusExprL   cls Focus.Class
    Focus.Function fun -> focusExprL   fun Focus.Function
    Focus.Lambda   lam -> focusExprL   lam Focus.Lambda
    where
        focusModuleL ast mkFocus = do
            (ast', lambda) <- replaceModuleByAstID ast (Expr.NOP astID) <?> "Zipper.focusLambda : Cannot find astID = " ++ show astID
            return (Focus.Lambda lambda, mkFocus ast' : path)

        focusExprL ast mkFocus = do
            (ast', lambda) <- replaceExprByAstID ast (Expr.NOP astID) <?> "Zipper.focusLambda : Cannot find astID = " ++ show astID
            return (Focus.Lambda lambda, mkFocus ast' : path)



focusCrumb :: Crumb -> Zipper -> Either Error Zipper
focusCrumb c = case c of
    Crumb.Module   name      -> focusModule   name
    Crumb.Class    name      -> focusClass    name
    Crumb.Function name path -> focusFunction name path
    Crumb.Lambda   astID     -> focusLambda   astID


focusBreadcrumbs :: Breadcrumbs -> Zipper -> Either Error Zipper
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> focusCrumb h zipper >>= focusBreadcrumbs t


focusModule' :: String -> Module -> Either Error Zipper
focusModule' name m = do
    assert (m ^. Module.cls . Type.name == name) $ "Zipper.focusModule' : Cannot focus on " ++ show name
    return $ mk m


focusCrumb' :: Crumb -> Module -> Either Error Zipper
focusCrumb' c m = case c of
    Crumb.Module name -> focusModule' name m
    _                 -> Left $ "Zipper.focusCrumb' : Cannot focus on " ++ show c


focusBreadcrumbs' :: Breadcrumbs -> Module -> Either Error Zipper
focusBreadcrumbs' bc m = case bc of
    h:t -> focusCrumb' h m >>= focusBreadcrumbs t
    _   -> Left $ "Zipper.focusBreadcrumbs' : Cannot focus on " ++ show bc


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
                Nothing   -> Left "Zipper.focusListElem : Cannot find element in AST."
    return (elemFocus func, crumbFocus newelem : path)


replaceExprByAstID :: Expr -> Expr -> Maybe (Expr, Expr)
replaceExprByAstID ast part = do
    let fexp expr = if expr ^. Expr.id == part ^. Expr.id
                        then put (Just expr) >> return part
                        else return expr
        (ast', mpart') = runState (Expr.traverseMR fexp return return return return ast) Nothing
    part' <- mpart'
    return (ast', part')


replaceModuleByAstID :: Module -> Expr -> Maybe (Module, Expr)
replaceModuleByAstID ast part = do
    let fexp expr = if expr ^. Expr.id == part ^. Expr.id
                        then put (Just expr) >> return part
                        else return expr
        (ast', mpart') = runState (Module.traverseMR return fexp return return return return ast) Nothing
    part' <- mpart'
    return (ast', part')
