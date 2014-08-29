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

import           Flowbox.Batch.Batch                     (Batch)
import           Flowbox.Batch.Handler.Common            (astClassFocusOp, astFocusOp, astFunctionFocusOp, astModuleFocusOp, astOp, libManagerOp)
import qualified Flowbox.Batch.Handler.Common            as Batch
import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude                         hiding (cons)
import           Flowbox.System.Log.Logger
import           Luna.AST.Control.Crumb                  (Breadcrumbs)
import           Luna.AST.Control.Focus                  (Focus)
import qualified Luna.AST.Control.Focus                  as Focus
import qualified Luna.AST.Control.Zipper                 as Zipper
import           Luna.AST.Expr                           (Expr)
import qualified Luna.AST.Expr                           as Expr
import           Luna.AST.Module                         (Module)
import qualified Luna.AST.Module                         as Module
import           Luna.AST.Type                           (Type)
import qualified Luna.AST.Type                           as Type
import qualified Luna.Graph.PropertyMap                  as PropertyMap
import qualified Luna.Lib.Lib                            as Library
import qualified Luna.Pass.Analysis.ID.ExtractIDs        as ExtractIDs
import qualified Luna.Pass.Analysis.NameResolver         as NameResolver
import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer as IDFixer
import qualified Luna.Pass.Transform.AST.Shrink          as Shrink



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
        Focus.Class    _ -> left "Cannot add module to a class"
        Focus.Function _ -> left "Cannot add module to a function"
        Focus.Module   m -> return $ Focus.Module $ Module.addModule fixedModule m
    return (newFocus, fixedModule))


addClass :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addClass newClass bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedClass <- EitherT $ IDFixer.runExpr maxID Nothing True newClass
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addClass fixedClass c
        Focus.Function _ -> left "Cannot add class to a function"
        Focus.Module   m -> return $ Focus.Module $ Module.addClass fixedClass m
    return (newFocus, fixedClass))


addFunction :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addFunction newFunction bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedFunction <- EitherT $ IDFixer.runExpr maxID Nothing True newFunction
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addMethod fixedFunction c
        Focus.Function _ -> left "Cannot add function to a function"
        Focus.Module   m -> return $ Focus.Module $ Module.addMethod fixedFunction m
    return (newFocus, fixedFunction))


remove :: Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
remove bc libID projectID = astOp libID projectID (\ast propertyMap -> do
    focus <- hoistEither $ Zipper.focusBreadcrumbs' bc ast
    ids   <- EitherT $ ExtractIDs.run $ Zipper.getFocus focus
    let newPropertyMap = foldr PropertyMap.delete propertyMap $ IntSet.toList ids
        newAst         = Zipper.close $ Zipper.defocusDrop focus
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
