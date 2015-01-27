---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}

module Luna.Syntax.Control.Zipper where

import qualified Data.List 
import qualified Data.List.Zipper as LZ

import           Flowbox.Control.Error     (assert, (<?>))
import           Flowbox.Prelude           hiding (drop, id, mod)
import           Luna.Syntax.Control.Crumb (Breadcrumbs, Crumb)
import qualified Luna.Syntax.Control.Crumb as Crumb
import           Luna.Syntax.Control.Focus (Focus, FocusPath)
import qualified Luna.Syntax.Control.Focus as Focus
import           Luna.Syntax.Decl          (LDecl)
import qualified Luna.Syntax.Decl          as Decl
import qualified Luna.Syntax.Label         as Label
import           Luna.Syntax.Module        (LModule)
import qualified Luna.Syntax.Module        as Module
import           Luna.Syntax.Name          (TNameP)
import           Luna.Syntax.Name.Path     (NamePath, QualPath)
import qualified Luna.Syntax.Name.Pattern as Pattern



type Zipper a v = (Focus a v, FocusPath a v)

type Error = String


mk :: LModule a v -> Zipper a v
mk rootmod = (Focus.Module rootmod, [])


defocus :: Zipper a v -> Zipper a v
defocus zipper@(_, [])     = zipper
defocus (env, parent:path) = (newenv, path) where
    newenv = case parent of
        Focus.Module   pmdl -> Focus.Module $ case env of
            Focus.Data     dat -> moduleAddDecl dat pmdl
            Focus.Function fun -> moduleAddDecl fun pmdl
            --Focus.Lambda   lam -> defocusModuleL pmdl lam
            --Focus.Module   mod -> Module.addModule mod pmdl
            _                  -> error "Zipper.defocus"
        Focus.Data     pdat -> Focus.Data  $ case env of
            Focus.Function fun -> dataAddDecl fun pdat
            Focus.Data     dat -> dataAddDecl dat pdat
            _                  -> error "Zipper.defocus"
        _                  -> error "Zipper.defocus"
        --Focus.Function pfun -> Focus.Function $ case env of
        --    Focus.Lambda   lam -> defocusExprL pfun lam
        --    _                  -> error "Zipper.defocus"
        --Focus.Lambda   plam -> Focus.Lambda $ case env of
        --    Focus.Lambda   lam -> defocusExprL plam lam
        --    _                  -> error "Zipper.defocus"
    moduleAddDecl :: LDecl a v -> LModule a v -> LModule a v
    moduleAddDecl dcl = Label.element . Module.body %~ (dcl:)

    dataAddDecl :: LDecl a v -> LDecl a v -> LDecl a v
    dataAddDecl dcl = Label.element . Decl.dataDecl . Decl.dataDeclDecls %~ (dcl:)

    --funAddDecl :: LDecl a v -> LDecl a v -> LDecl a v
    --funAddDecl dcl = Label.element . Decl.funcDecl . Decl.?? %~ (dcl:)

    --defocusModuleL par lam = case replaceModuleByAstID par lam of
    --    Nothing        -> error "Zipper.defocus"
    --    Just (par', _) -> par'
    --defocusExprL par lam   = case replaceExprByAstID par lam of
    --    Nothing        -> error "Zipper.defocus"
    --    Just (par', _) -> par'


defocusDrop :: Zipper a v -> Zipper a v
defocusDrop zipper@(_, [])   = zipper
defocusDrop (_, parent:path) = (parent, path)


modify :: (Focus a v -> Focus a v) -> Zipper a v -> Zipper a v
modify f (env, path) = (f env, path)


close :: Zipper a v -> LModule a v
close (env, []) = mod where Focus.Module mod = env
close zipper    = close $ defocus zipper


focusModule :: QualPath -> Zipper a v -> Either Error (Zipper a v)
focusModule mpath (env, _) = case env of
    _                -> Left $ "Zipper.focusModule : Cannot focus on " ++ show mpath


focusClass :: (Show v, Show a) => TNameP -> Zipper a v -> Either Error (Zipper a v)
focusClass name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem (Label.element . Module.body) classComp
                            Focus.Data  Focus.Module mod zipper
    Focus.Data   cls -> focusListElem (Label.element . Decl.dataDecl . Decl.dataDeclDecls) classComp
                            Focus.Data  Focus.Data   cls zipper
    _                -> Left $ "Zipper.focusClass : Cannot focus on " ++ show name
    where
        classComp :: (Show e00, Show a00, Show l00) => Label.Label l00 (Decl.Decl a00 e00) -> Bool
        classComp f = f ^? (Label.element . Decl.dataDecl . Decl.dataDeclName) == Just name


focusFunction :: (Show v, Show a) => Decl.Path -> NamePath -> Zipper a v -> Either Error (Zipper a v)
focusFunction path name zipper@(env, _) = case env of
    Focus.Module mod -> focusListElem (Label.element . Module.body) funComp
                            Focus.Function Focus.Module mod zipper
    Focus.Data   cls -> focusListElem (Label.element . Decl.dataDecl . Decl.dataDeclDecls) funComp
                            Focus.Function Focus.Data  cls zipper
    _                -> Left $ "Zipper.focusFunction : Cannot focus on " ++ show path
    where funComp f = (f ^? (Label.element . Decl.funcDecl . Decl.funcDeclPath) == Just path)
                   && (f ^? (Label.element . Decl.funcDecl . Decl.funcDeclSig . to Pattern.toNamePath) == Just name)


--focusLambda :: AST.ID -> Zipper -> Either Error Zipper
--focusLambda astID (env, path) = case env of
--    Focus.Module   mod -> focusModuleL mod Focus.Module
--    Focus.Data     cls -> focusExprL   cls Focus.Data
--    Focus.Function fun -> focusExprL   fun Focus.Function
--    Focus.Lambda   lam -> focusExprL   lam Focus.Lambda
--    where
--        focusModuleL ast mkFocus = do
--            (ast', lambda) <- replaceModuleByAstID ast (Expr.NOP astID) <?> "Zipper.focusLambda : Cannot find astID = " ++ show astID
--            return (Focus.Lambda lambda, mkFocus ast' : path)

--        focusExprL ast mkFocus = do
--            (ast', lambda) <- replaceExprByAstID ast (Expr.NOP astID) <?> "Zipper.focusLambda : Cannot find astID = " ++ show astID
--            return (Focus.Lambda lambda, mkFocus ast' : path)



focusCrumb :: (Show a, Show v) => Crumb -> Zipper a v -> Either Error (Zipper a v)
focusCrumb c = case c of
    Crumb.Module   mpath     -> focusModule   mpath
    Crumb.Class    className -> focusClass    className
    Crumb.Function path name -> focusFunction path name
    _                        -> const $ Left "Zipper.focusCrumb : Cannot focus on lambda (not implemented)"
    --Crumb.Lambda   astID     -> focusLambda   astID


focusBreadcrumbs :: (Show a, Show v) => Breadcrumbs -> Zipper a v -> Either Error (Zipper a v)
focusBreadcrumbs bc zipper = case bc of
    []  -> return zipper
    h:t -> focusCrumb h zipper >>= focusBreadcrumbs t


focusModule' :: QualPath -> LModule a v -> Either Error (Zipper a v)
focusModule' mpath m = do
    assert (m ^. Label.element . Module.mpath == mpath) $ "Zipper.focusModule' : Cannot focus on " ++ show mpath
    return $ mk m


focusCrumb' :: Crumb -> LModule a v -> Either Error (Zipper a v)
focusCrumb' c m = case c of
    Crumb.Module name -> focusModule' name m
    _                 -> Left $ "Zipper.focusCrumb' : Cannot focus on " ++ show c


focusBreadcrumbs' :: (Show a, Show v) => Breadcrumbs -> LModule a v -> Either Error (Zipper a v)
focusBreadcrumbs' bc m = case bc of
    h:t -> focusCrumb' h m >>= focusBreadcrumbs t
    _   -> Left $ "Zipper.focusBreadcrumbs' : Cannot focus on " ++ show bc


getFocus :: Zipper a v -> Focus a v
getFocus = fst


focusListElem :: Traversal' t [u] -> (u -> Bool) -> (u -> Focus a v) -> (t -> Focus a v) -> t -> Zipper a v -> Either Error (Zipper a v)
focusListElem flens match elemFocus crumbFocus el (_, path) = do
    let funcs    = el ^. flens
        mfunc    = List.find match funcs
        newfuncs = filter (not . match) funcs
        newelem  = el & flens .~ newfuncs
    func <- mfunc <?> "Zipper.focusListElem : Cannot find element in AST."
    return (elemFocus func, crumbFocus newelem : path)


findZ :: Eq a => a -> [a] -> LZ.Zipper a
findZ = undefined


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
