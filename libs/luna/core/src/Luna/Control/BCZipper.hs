---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeOperators             #-}

module Luna.Control.BCZipper where

import           Control.Zipper           ((:>>), Top)
import qualified Control.Zipper           as Zipper
import qualified Data.List                as List

import           Flowbox.Control.Error    (assert, (<?>))
import           Flowbox.Prelude          hiding (drop, id, mod)
import           Luna.Control.Crumb       (Breadcrumbs, Crumb)
import qualified Luna.Control.Crumb       as Crumb
import           Luna.Control.Focus       (Focus, FocusPath)
import qualified Luna.Control.Focus       as Focus
import           Luna.Syntax.Decl         (LDecl)
import qualified Luna.Syntax.Decl         as Decl
import qualified Luna.Syntax.Label        as Label
import           Luna.Syntax.Module       (LModule)
import qualified Luna.Syntax.Module       as Module
import           Luna.Syntax.Name         (TNameP)
import           Luna.Syntax.Name.Path    (NamePath, QualPath)
import qualified Luna.Syntax.Name.Pattern as Pattern


type BCZipper a v = (Focus a v, FocusPath a v)

type Error = String


mk :: LModule a v -> BCZipper a v
mk rootmod = (Focus.Module rootmod, [])


defocus :: BCZipper a v -> BCZipper a v
defocus zipper@(_, [])     = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        Focus.ModuleZ   pmdl -> Focus.Module $ Zipper.rezip $ case env of
            Focus.Data     dat -> pmdl & Zipper.focus .~ dat
            Focus.Function fun -> pmdl & Zipper.focus .~ fun
            --Focus.Lambda   lam -> defocusModuleL pmdl lam
            --Focus.Module   mod -> Module.addModule mod pmdl
            _                  -> error "BCZipper.defocus"
        Focus.DataZ     pdat -> Focus.Data $ Zipper.rezip $ case env of
            Focus.Data     dat -> pdat & Zipper.focus .~ dat
            Focus.Function fun -> pdat & Zipper.focus .~ fun
            _                  -> error "BCZipper.defocus"
        --Focus.Function pfun -> Focus.Function $ case env of
        --    Focus.Lambda   lam -> defocusExprL pfun lam
        --    _                  -> error "BCZipper.defocus"
        --Focus.Lambda   plam -> Focus.Lambda $ case env of
        --    Focus.Lambda   lam -> defocusExprL plam lam
        --    _                  -> error "BCZipper.defocus"

    --funAddDecl :: LDecl a v -> LDecl a v -> LDecl a v
    --funAddDecl dcl = Label.element . Decl.funcDecl . Decl.?? %~ (dcl:)

    --defocusModuleL par lam = case replaceModuleByAstID par lam of
    --    Nothing        -> error "BCZipper.defocus"
    --    Just (par', _) -> par'
    --defocusExprL par lam   = case replaceExprByAstID par lam of
    --    Nothing        -> error "BCZipper.defocus"
    --    Just (par', _) -> par'


--defocusDrop :: BCZipper a v -> BCZipper a v
--defocusDrop zipper@(_, [])   = zipper
--defocusDrop (_, parent:path) = (parent, path)


modify :: (Focus a v -> Focus a v) -> BCZipper a v -> BCZipper a v
modify f (env, path) = (f env, path)


close :: BCZipper a v -> LModule a v
close (env, []) = mod where Focus.Module mod = env
close zipper    = close $ defocus zipper


focusModule :: QualPath -> BCZipper a v -> Either Error (BCZipper a v)
focusModule mpath (env, _) = case env of
    _                -> Left $ "BCZipper.focusModule : Cannot focus on " ++ show mpath


focusClass :: (Show v, Show a) => TNameP -> BCZipper a v -> Either Error (BCZipper a v)
focusClass name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem (Label.element . Module.body) classComp
                            Focus.Data  Focus.ModuleZ mod zipper
    Focus.Data   cls -> focusListElem (Label.element . Decl.dataDecl . Decl.dataDeclDecls) classComp
                            Focus.Data  Focus.DataZ   cls zipper
    _                -> Left $ "BCZipper.focusClass : Cannot focus on " ++ show name
    where
        classComp :: (Show e00, Show a00, Show l00) => Label.Label l00 (Decl.Decl a00 e00) -> Bool
        classComp f = f ^? (Label.element . Decl.dataDecl . Decl.dataDeclName) == Just name


focusFunction :: (Show v, Show a) => Decl.Path -> NamePath -> BCZipper a v -> Either Error (BCZipper a v)
focusFunction path name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem (Label.element . Module.body) funComp
                            Focus.Function Focus.ModuleZ mod zipper
    Focus.Data   cls -> focusListElem (Label.element . Decl.dataDecl . Decl.dataDeclDecls) funComp
                            Focus.Function Focus.DataZ  cls zipper
    _                -> Left $ "BCZipper.focusFunction : Cannot focus on " ++ show path
    where funComp f = (f ^? (Label.element . Decl.funcDecl . Decl.funcDeclPath) == Just path)
                   && (f ^? (Label.element . Decl.funcDecl . Decl.funcDeclSig . to Pattern.toNamePath) == Just name)


--focusLambda :: AST.ID -> BCZipper -> Either Error BCZipper
--focusLambda astID (env, path) = case env of
--    Focus.Module   mod -> focusModuleL mod Focus.Module
--    Focus.Data     cls -> focusExprL   cls Focus.Data
--    Focus.Function fun -> focusExprL   fun Focus.Function
--    Focus.Lambda   lam -> focusExprL   lam Focus.Lambda
--    where
--        focusModuleL ast mkFocus = do
--            (ast', lambda) <- replaceModuleByAstID ast (Expr.NOP astID) <?> "BCZipper.focusLambda : Cannot find astID = " ++ show astID
--            return (Focus.Lambda lambda, mkFocus ast' : path)

--        focusExprL ast mkFocus = do
--            (ast', lambda) <- replaceExprByAstID ast (Expr.NOP astID) <?> "BCZipper.focusLambda : Cannot find astID = " ++ show astID
--            return (Focus.Lambda lambda, mkFocus ast' : path)



focusCrumb :: (Show a, Show v) => Crumb -> BCZipper a v -> Either Error (BCZipper a v)
focusCrumb c = case c of
    Crumb.Module   mpath     -> focusModule   mpath
    Crumb.Class    className -> focusClass    className
    Crumb.Function path name -> focusFunction path name
    _                        -> const $ Left "BCZipper.focusCrumb : Cannot focus on lambda (not implemented)"
    --Crumb.Lambda   astID     -> focusLambda   astID


focusBreadcrumbs :: (Show a, Show v) => Breadcrumbs -> BCZipper a v -> Either Error (BCZipper a v)
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> focusCrumb h zipper >>= focusBreadcrumbs t


focusModule' :: QualPath -> LModule a v -> Either Error (BCZipper a v)
focusModule' mpath m = do
    assert (m ^. Label.element . Module.mpath == mpath) $ "BCZipper.focusModule' : Cannot focus on " ++ show mpath
    return $ mk m


focusCrumb' :: Crumb -> LModule a v -> Either Error (BCZipper a v)
focusCrumb' c m = case c of
    Crumb.Module name -> focusModule' name m
    _                 -> Left $ "BCZipper.focusCrumb' : Cannot focus on " ++ show c


focusBreadcrumbs' :: (Show a, Show v) => Breadcrumbs -> LModule a v -> Either Error (BCZipper a v)
focusBreadcrumbs' bc m = case bc of
    h:t -> focusCrumb' h m >>= focusBreadcrumbs t
    _   -> Left $ "BCZipper.focusBreadcrumbs' : Cannot focus on " ++ show bc


getFocus :: BCZipper a v -> Focus a v
getFocus = fst


--focusListElem :: Traversal' (LDecl a v) [LDecl a v] -> (LDecl a v -> Bool) -> (LDecl a v -> Focus a v) -> (t -> Focus.FocusZ a v) -> LDecl a v -> BCZipper a v -> Either Error (BCZipper a v)
--focusListElem flens match elemFocus crumbFocus el (_, path) = do
--    funcIx  <- List.findIndex match (el ^. flens) <?> "BCZipper.focusListElem : Cannot find element in AST."
--    z       <- Zipper.within (flens . ix funcIx) (Zipper.zipper el) <?> "BCZipper.focusListElem : Cannot find element in AST."
--    let func = z ^. Zipper.focus
--    return (elemFocus func, Focus.DataZ z : path)


--focusListElem' :: Traversal' (LModule a v) [LDecl a v] -> (LDecl a v -> Bool) -> (LDecl a v -> Focus a v) -> (t -> Focus.FocusZ a v) -> LModule a v -> BCZipper a v -> Either Error (BCZipper a v)
--focusListElem' flens match elemFocus crumbFocus el (_, path) = do
--    funcIx  <- List.findIndex match (el ^. flens) <?> "BCZipper.focusListElem : Cannot find element in AST."
--    z       <- Zipper.within (flens . ix funcIx) (Zipper.zipper el) <?> "BCZipper.focusListElem : Cannot find element in AST."
--    let func = z ^. Zipper.focus
--    return (elemFocus func, Focus.ModuleZ z : path)

focusListElem :: Traversal' u [LDecl a v]
              -> (LDecl a v -> Bool)
              -> (LDecl a v -> Focus a v)
              -> (Top :>> u :>> LDecl a v -> Focus.FocusZ a v)
              -> u -> BCZipper a v -> Either Error (BCZipper a v)
focusListElem flens match elemFocus crumbFocus el (_, path) = do
    funcIx  <- List.findIndex match (el ^. flens) <?> "BCZipper.focusListElem : Cannot find element in AST."
    z       <- Zipper.within (flens . ix funcIx) (Zipper.zipper el) <?> "BCZipper.focusListElem : Cannot find element in AST."
    let func = z ^. Zipper.focus
    return (elemFocus func, crumbFocus z : path)

--replaceExprByAstID :: Expr -> Expr -> Maybe (Expr, Expr)
--replaceExprByAstID ast part = do
--    let fexp expr = if expr ^. Expr.id == part ^. Expr.id
--                        then put (Just expr) >> return part
--                        else return expr
--        (ast', mpart') = runState (Expr.traverseMR fexp return return return return ast) Nothing
--    part' <- mpart'
--    return (ast', part')


--replaceModuleByAstID :: Module -> Expr -> Maybe (Module, Expr)
--replaceModuleByAstID ast part = do
--    let fexp expr = if expr ^. Expr.id == part ^. Expr.id
--                        then put (Just expr) >> return part
--                        else return expr
--        (ast', mpart') = runState (Module.traverseMR return fexp return return return return ast) Nothing
--    part' <- mpart'
--    return (ast', part')
