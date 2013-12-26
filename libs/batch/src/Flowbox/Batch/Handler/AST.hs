---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.AST where

import qualified Data.IntSet as IntSet

import           Flowbox.Batch.Batch                        (Batch)
import           Flowbox.Batch.Handler.Common               (astClassFocusOp, astFocusOp, astFunctionFocusOp, astModuleFocusOp, astOp, noresult, readonly)
import qualified Flowbox.Batch.Project.Project              as Project
import           Flowbox.Luna.Data.AST.Crumb.Crumb          (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Expr                 (Expr)
import qualified Flowbox.Luna.Data.AST.Expr                 as Expr
import           Flowbox.Luna.Data.AST.Module               (Module)
import qualified Flowbox.Luna.Data.AST.Module               as Module
import           Flowbox.Luna.Data.AST.Type                 (Type)
import           Flowbox.Luna.Data.AST.Zipper.Focus         (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus         as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper        as Zipper
import qualified Flowbox.Luna.Data.PropertyMap              as PropertyMap
import qualified Flowbox.Luna.Lib.Library                   as Library
import qualified Flowbox.Luna.Passes.Analysis.ID.ExtractIDs as ExtractIDs
import qualified Flowbox.Luna.Passes.General.Luna.Luna      as Luna
import qualified Flowbox.Luna.Passes.Transform.AST.Shrink   as Shrink
import           Flowbox.Prelude                            hiding (focus)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.AST"


definitions :: Maybe Int -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Focus
definitions mmaxDepth bc libID projectID = readonly . astFocusOp bc libID projectID (\_ focus -> do
    shrinked <- Shrink.shrinkFunctionBodies focus
    return (focus, shrinked))



addModule :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
addModule newModule bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Focus.ClassFocus    _ -> fail "Cannot add module to a class"
        Focus.FunctionFocus _ -> fail "Cannot add module to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addModule newModule m
    return (newFocus , ()))


addClass :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
addClass newClass bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Focus.ClassFocus    c -> return $ Focus.ClassFocus $ Expr.addClass newClass c
        Focus.FunctionFocus _ -> fail "Cannot add class to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addClass newClass m
    return (newFocus, ()))


addFunction :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
addFunction newFunction bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Focus.ClassFocus    c -> return $ Focus.ClassFocus $ Expr.addMethod newFunction c
        Focus.FunctionFocus _ -> fail "Cannot add function to a function"
        Focus.ModuleFocus   m -> return $ Focus.ModuleFocus $ Module.addMethod newFunction m
    return (newFocus, ()))


remove :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
remove bc libID projectID = noresult . astOp libID projectID (\_ ast propertyMap -> do
    focus <- Zipper.mk ast >>= Zipper.focusBreadcrumbs bc
    ids   <- Luna.runIO $ ExtractIDs.run $ Zipper.getFocus focus
    let newPropertyMap = foldr PropertyMap.delete propertyMap $ IntSet.toList ids
    newAst <- Zipper.close $ Zipper.defocusDrop focus
    return ((newAst, newPropertyMap), ()))


updateModuleCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateModuleCls cls bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.cls .~ cls, ()))


updateModuleImports :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateModuleImports imports bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.imports .~ imports, ()))


updateModuleFields :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateModuleFields fields bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.fields .~ fields, ()))


updateClassCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateClassCls cls bc libID projectID = noresult . astClassFocusOp bc libID projectID (\_ m ->
    return (m & Expr.cls .~ cls, ()))


updateClassFields :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateClassFields fields bc libID projectID = noresult . astClassFocusOp bc libID projectID (\_ m ->
    return (m & Expr.fields .~ fields, ()))


updateFunctionName :: String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateFunctionName name bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.name .~ name, ()))


updateFunctionPath :: [String] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateFunctionPath path bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.path .~ path, ()))


updateFunctionInputs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateFunctionInputs inputs bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.inputs .~ inputs, ()))


updateFunctionOutput :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
updateFunctionOutput output bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.output .~ output, ()))
