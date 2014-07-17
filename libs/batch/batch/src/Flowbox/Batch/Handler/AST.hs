---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.AST where

import qualified Data.IntSet as IntSet
import qualified Data.Tuple  as Tuple

import           Flowbox.Batch.Batch                               (Batch)
import           Flowbox.Batch.Handler.Common                      (astClassFocusOp, astFocusOp, astFunctionFocusOp, astModuleFocusOp, astOp, libManagerOp)
import qualified Flowbox.Batch.Handler.Common                      as Batch
import qualified Flowbox.Batch.Project.Project                     as Project
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs           (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Expr                        (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Module                      (Module)
import qualified Flowbox.Luna.Data.AST.Module                      as Module
import           Flowbox.Luna.Data.AST.Type                        (Type)
import qualified Flowbox.Luna.Data.AST.Type                        as Type
import           Flowbox.Luna.Data.AST.Zipper.Focus                (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper               as Zipper
import qualified Flowbox.Luna.Data.PropertyMap                     as PropertyMap
import qualified Flowbox.Luna.Lib.Library                          as Library
import qualified Flowbox.Luna.Passes.Analysis.ID.ExtractIDs        as ExtractIDs
import qualified Flowbox.Luna.Passes.Analysis.NameResolver         as NameResolver
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer as IDFixer
import qualified Flowbox.Luna.Passes.Transform.AST.Shrink          as Shrink
import           Flowbox.Prelude                                   hiding (cons)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Batch.Handler.AST"


definitions :: Maybe Int -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Focus
definitions mmaxDepth bc libID projectID = astFocusOp bc libID projectID (\focus -> do
    shrinked <- Shrink.shrinkFunctionBodies focus
    return (focus, shrinked))


addModule :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Module
addModule newModule bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedModule <- EitherT $ IDFixer.runModule maxID Nothing True newModule
    newFocus    <- case focus of
        Focus.ClassFocus    _ -> fail "Cannot add module to a class"
        Focus.FunctionFocus _ -> fail "Cannot add module to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addModule fixedModule m
    return (newFocus, fixedModule))


addClass :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addClass newClass bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedClass <- EitherT $ IDFixer.runExpr maxID Nothing True newClass
    newFocus <- case focus of
        Focus.ClassFocus    c -> return $ Focus.ClassFocus $ Expr.addClass fixedClass c
        Focus.FunctionFocus _ -> fail "Cannot add class to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addClass fixedClass m
    return (newFocus, fixedClass))


addFunction :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addFunction newFunction bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedFunction <- EitherT $ IDFixer.runExpr maxID Nothing True newFunction
    newFocus <- case focus of
        Focus.ClassFocus    c -> return $ Focus.ClassFocus $ Expr.addMethod fixedFunction c
        Focus.FunctionFocus _ -> fail "Cannot add function to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addMethod fixedFunction m
    return (newFocus, fixedFunction))


remove :: Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
remove bc libID projectID = astOp libID projectID (\ast propertyMap -> do
    focus <- Zipper.focusBreadcrumbs' bc ast
    ids   <- EitherT $ ExtractIDs.run $ Zipper.getFocus focus
    let newPropertyMap = foldr PropertyMap.delete propertyMap $ IntSet.toList ids
    newAst <- Zipper.close $ Zipper.defocusDrop focus
    return ((newAst, newPropertyMap), ()))


resolveDefinition :: String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch [(Breadcrumbs, Library.ID)]
resolveDefinition name bc libID projectID = libManagerOp projectID (\libManager -> do
    results <- EitherT $ NameResolver.run name bc libID libManager
    return (libManager, map Tuple.swap results))


updateModuleCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateModuleCls cls bc libID projectID = astModuleFocusOp bc libID projectID (\m -> do
    maxID    <- Batch.getMaxID libID projectID
    fixedCls <- EitherT $ IDFixer.runType maxID (Just $ m ^. Module.cls . Type.id) True cls
    return (m & Module.cls .~ fixedCls, ()))


updateModuleImports :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateModuleImports imports bc libID projectID = astModuleFocusOp bc libID projectID (\m -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedImports <- EitherT $ IDFixer.runExprs maxID Nothing True imports
    return (m & Module.imports .~ fixedImports, ()))


updateModuleFields :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateModuleFields fields bc libID projectID = astModuleFocusOp bc libID projectID (\m -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedFields <- EitherT $ IDFixer.runExprs maxID Nothing True fields
    return (m & Module.fields .~ fixedFields, ()))


updateDataCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateDataCls cls bc libID projectID = astClassFocusOp bc libID projectID (\m -> do
    maxID    <- Batch.getMaxID libID projectID
    fixedCls <- EitherT $ IDFixer.runType maxID (Just $ m ^?! Expr.cls . Type.id) True cls
    return (m & Expr.cls .~ fixedCls, ()))


updateDataCons :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateDataCons cons bc libID projectID = astClassFocusOp bc libID projectID (\m -> do
    maxID     <- Batch.getMaxID libID projectID
    fixedCons <- EitherT $ IDFixer.runExprs maxID Nothing True cons
    return (m & Expr.cons .~ fixedCons, ()))


updateDataClasses :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateDataClasses classes bc libID projectID = astClassFocusOp bc libID projectID (\m -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedClasses <- EitherT $ IDFixer.runExprs maxID Nothing True classes
    return (m & Expr.classes .~ fixedClasses, ()))


updateDataMethods :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateDataMethods methods bc libID projectID = astClassFocusOp bc libID projectID (\m -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedMethods <- EitherT $ IDFixer.runExprs maxID Nothing True methods
    return (m & Expr.methods .~ fixedMethods, ()))


updateFunctionName :: String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateFunctionName name bc libID projectID = astFunctionFocusOp bc libID projectID (\m ->
    return (m & Expr.name .~ name, ()))


updateFunctionPath :: [String] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateFunctionPath path bc libID projectID = astFunctionFocusOp bc libID projectID (\m ->
    return (m & Expr.path .~ path, ()))


updateFunctionInputs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateFunctionInputs inputs bc libID projectID = astFunctionFocusOp bc libID projectID (\m -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedInputs <- EitherT $ IDFixer.runExprs maxID Nothing True inputs
    return (m & Expr.inputs .~ fixedInputs, ()))


updateFunctionOutput :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
updateFunctionOutput output bc libID projectID = do
    function    <- Batch.getFunctionFocus bc libID projectID
    propertyMap <- Batch.getPropertyMap libID projectID

    let oldID = function ^?! Expr.output . Type.id

    maxID       <- Batch.getMaxID libID projectID
    fixedOutput <- EitherT $ IDFixer.runType maxID (Just oldID) True output

    let newID = fixedOutput ^. Type.id
        newFunction    = function & Expr.output .~ fixedOutput
        newPropertyMap = PropertyMap.move (-oldID) (-newID) propertyMap

    Batch.setFunctionFocus newFunction bc libID projectID
    Batch.setPropertyMap newPropertyMap libID projectID
