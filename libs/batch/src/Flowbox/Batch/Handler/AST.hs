---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.AST where

import           Flowbox.Batch.Batch                      (Batch)
import           Flowbox.Batch.Handler.Common             (astClassFocusOp, astFocusOp, astFunctionFocusOp, astModuleFocusOp, astOp, noresult, readonly)
import qualified Flowbox.Batch.Project.Project            as Project
import           Flowbox.Luna.Data.AST.Crumb.Crumb        (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Expr               (Expr)
import qualified Flowbox.Luna.Data.AST.Expr               as Expr
import           Flowbox.Luna.Data.AST.Module             (Module)
import qualified Flowbox.Luna.Data.AST.Module             as Module
import           Flowbox.Luna.Data.AST.Type               (Type)
import qualified Flowbox.Luna.Data.AST.Zipper             as Zipper
import qualified Flowbox.Luna.Lib.Library                 as Library
import qualified Flowbox.Luna.Passes.Transform.AST.Shrink as Shrink
import           Flowbox.Prelude                          hiding (focus)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.AST"


definitions :: Int -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Module
definitions maxDepth bc libID projectID = readonly . astOp libID projectID (\_ ast -> do
    loggerIO warning "maxDepth and breadcrumbs are not yet implemented. Returning whole AST from root."
    shrinked <- Shrink.shrinkFunctionBodies ast
    return (ast, shrinked))


addModule :: (Applicative m, Monad m)
          => Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
addModule newModule bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Zipper.ClassFocus    _ -> fail "Cannot add module to a class"
        Zipper.FunctionFocus _ -> fail "Cannot add module to a function"
        Zipper.ModuleFocus   m -> return $ Zipper.ModuleFocus $ Module.addModule newModule m
    return (newFocus , ()))


addClass :: (Applicative m, Monad m)
          => Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
addClass newClass bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Zipper.ClassFocus    c -> return $ Zipper.ClassFocus $ Expr.addClass newClass c
        Zipper.FunctionFocus _ -> fail "Cannot add class to a function"
        Zipper.ModuleFocus   m -> return $ Zipper.ModuleFocus $ Module.addClass newClass m
    return (newFocus, ()))


addFunction :: (Applicative m, Monad m)
            => Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
addFunction newFunction bcParent libID projectID = noresult . astFocusOp bcParent libID projectID (\_ focus -> do
    newFocus <- case focus of
        Zipper.ClassFocus    c -> return $ Zipper.ClassFocus $ Expr.addMethod newFunction c
        Zipper.FunctionFocus _ -> fail "Cannot add function to a function"
        Zipper.ModuleFocus   m -> return $ Zipper.ModuleFocus $ Module.addMethod newFunction m
    return (newFocus, ()))


remove :: (Applicative m, Monad m)
       => Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
remove bc libID projectID = noresult . astOp libID projectID (\_ ast -> do
    newAst <- Zipper.mk ast >>= Zipper.focusBreadcrumbs bc >>= Zipper.close . Zipper.defocusDrop
    return (newAst, ()))


updateModuleCls :: (Applicative m, Monad m)
                => Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateModuleCls cls bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.cls .~ cls, ()))


updateModuleImports :: (Applicative m, Monad m)
                    => [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateModuleImports imports bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.imports .~ imports, ()))


updateModuleFields :: (Applicative m, Monad m)
                   => [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateModuleFields fields bc libID projectID = noresult . astModuleFocusOp bc libID projectID (\_ m ->
    return (m & Module.fields .~ fields, ()))


updateClassCls :: (Applicative m, Monad m)
               => Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateClassCls cls bc libID projectID = noresult . astClassFocusOp bc libID projectID (\_ m ->
    return (m & Expr.cls .~ cls, ()))


updateClassFields :: (Applicative m, Monad m)
                  => [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateClassFields fields bc libID projectID = noresult . astClassFocusOp bc libID projectID (\_ m ->
    return (m & Expr.fields .~ fields, ()))


updateFunctionName :: (Applicative m, Monad m)
                   => String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateFunctionName name bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.name .~ name, ()))


updateFunctionPath :: (Applicative m, Monad m)
                   => [String] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateFunctionPath path bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.path .~ path, ()))


updateFunctionInputs :: (Applicative m, Monad m)
                   => [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateFunctionInputs inputs bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.inputs .~ inputs, ()))


updateFunctionOutput :: (Applicative m, Monad m)
                   => Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
updateFunctionOutput output bc libID projectID = noresult . astFunctionFocusOp bc libID projectID (\_ m ->
    return (m & Expr.output .~ output, ()))
