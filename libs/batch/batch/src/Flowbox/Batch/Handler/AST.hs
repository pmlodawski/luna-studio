---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Handler.AST where

import qualified Data.IntSet as IntSet
import qualified Data.List   as List
import qualified Data.Tuple  as Tuple

import           Flowbox.Batch.Batch                         (Batch)
import           Flowbox.Batch.Handler.Common                (astDataConOp, astDataOp, astFocusOp, astFunctionOp, astModuleOp, astOp)
import qualified Flowbox.Batch.Handler.Common                as Batch
import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude                             hiding (cons)
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Common                         as AST
import           Luna.DEP.AST.Control.Crumb                  (Breadcrumbs)
import           Luna.DEP.AST.Control.Focus                  (Focus)
import qualified Luna.DEP.AST.Control.Focus                  as Focus
import qualified Luna.DEP.AST.Control.Zipper                 as Zipper
import           Luna.DEP.AST.Expr                           (Expr)
import qualified Luna.DEP.AST.Expr                           as Expr
import           Luna.DEP.AST.Module                         (Module)
import qualified Luna.DEP.AST.Module                         as Module
import           Luna.DEP.AST.Name                           (Name)
import           Luna.DEP.AST.Type                           (Type)
import qualified Luna.DEP.AST.Type                           as Type
import qualified Luna.DEP.Graph.PropertyMap                  as PropertyMap
import qualified Luna.DEP.Lib.Lib                            as Library
import qualified Luna.DEP.Pass.Analysis.ID.ExtractIDs        as ExtractIDs
import qualified Luna.DEP.Pass.Analysis.NameResolver         as NameResolver
import qualified Luna.DEP.Pass.Transform.AST.IDFixer.IDFixer as IDFixer
import qualified Luna.DEP.Pass.Transform.AST.Shrink          as Shrink



logger :: LoggerIO
logger = getLoggerIO $moduleName


definitions :: Maybe Int -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Focus
definitions mmaxDepth bc libraryID projectID = astFocusOp bc libraryID projectID (\focus -> do
    shrinked <- Shrink.shrinkFunctionBodies focus
    return (focus, shrinked))


addModule :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Module
addModule newModule bcParent libraryID projectID = astFocusOp bcParent libraryID projectID (\focus -> do
    astInfo     <- Batch.getASTInfo libraryID projectID
    (fixedModule, astInfo') <- EitherT $ IDFixer.runModule astInfo Nothing True newModule
    Batch.setASTInfo astInfo' libraryID projectID
    newFocus    <- case focus of
        Focus.Class    _ -> left "Cannot add module to a class"
        Focus.Function _ -> left "Cannot add module to a function"
        Focus.Lambda   _ -> left "Cannot add module to a lambda"
        Focus.Module   m -> return $ Focus.Module $ Module.addModule fixedModule m
    return (newFocus, fixedModule))


addData :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addData newClass bcParent libraryID projectID = astFocusOp bcParent libraryID projectID (\focus -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedClass, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing True newClass
    Batch.setASTInfo astInfo' libraryID projectID
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addClass fixedClass c
        Focus.Function _ -> left "Cannot add class to a function"
        Focus.Lambda   _ -> left "Cannot add class to a lambda"
        Focus.Module   m -> return $ Focus.Module $ Module.addClass fixedClass m
    return (newFocus, fixedClass))


addFunction :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addFunction newFunction bcParent libraryID projectID = astFocusOp bcParent libraryID projectID (\focus -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedFunction, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing True newFunction
    Batch.setASTInfo astInfo' libraryID projectID
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addMethod fixedFunction c
        Focus.Function _ -> left "Cannot add function to a function"
        Focus.Lambda   _ -> left "Cannot add function to a lambda"
        Focus.Module   m -> return $ Focus.Module $ Module.addMethod fixedFunction m
    return (newFocus, fixedFunction))


remove :: Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
remove bc libraryID projectID = astOp libraryID projectID (\ast propertyMap -> do
    focus <- hoistEither $ Zipper.focusBreadcrumbs' bc ast
    ids   <- EitherT $ ExtractIDs.run $ Zipper.getFocus focus
    let newPropertyMap = foldr PropertyMap.delete propertyMap $ IntSet.toList ids
        newAst         = Zipper.close $ Zipper.defocusDrop focus
    return ((newAst, newPropertyMap), ()))


resolveDefinition :: String -> Breadcrumbs -> Library.ID -> Project.ID -> Batch [(Breadcrumbs, Library.ID)]
resolveDefinition name bc libraryID projectID = do
    libManager <- Batch.getLibManager projectID
    results <- EitherT $ NameResolver.run name bc libraryID libManager
    return (map Tuple.swap results)


modifyModuleCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleCls cls bc libraryID projectID = astModuleOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedCls, astInfo') <- EitherT $ IDFixer.runType astInfo (Just $ m ^. Module.cls . Type.id) True cls
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Module.cls .~ fixedCls, ())


modifyModuleImports :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleImports imports bc libraryID projectID = astModuleOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedImports, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True imports
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Module.imports .~ fixedImports, ())


modifyModuleTypeAliases :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleTypeAliases typeAliases bc libraryID projectID = astModuleOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedTypeAliases, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True typeAliases
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Module.typeAliases .~ fixedTypeAliases, ())


modifyModuleTypeDefs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleTypeDefs typeDefs bc libraryID projectID = astModuleOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedTypeDefs, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True typeDefs
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Module.typeDefs .~ fixedTypeDefs, ())


modifyModuleFields :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleFields fields bc libraryID projectID = astModuleOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedFields, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True fields
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Module.fields .~ fixedFields, ())


modifyDataCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCls cls bc libraryID projectID = astDataOp bc libraryID projectID (\m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedCls, astInfo') <- EitherT $ IDFixer.runType astInfo (Just $ m ^?! Expr.cls . Type.id) True cls
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Expr.cls .~ fixedCls, ()))


modifyDataCons :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCons cons bc libraryID projectID = astDataOp bc libraryID projectID $ \m -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedCons, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True cons
    Batch.setASTInfo astInfo' libraryID projectID
    return (m & Expr.cons .~ fixedCons, ())


addDataCon :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch AST.ID
addDataCon con bc libraryID projectID = astDataOp bc libraryID projectID $ \data_ -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedCon, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing True con
    Batch.setASTInfo astInfo' libraryID projectID
    return (data_ & Expr.cons %~ (fixedCon:), fixedCon ^. Expr.id)


deleteDataCon :: AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
deleteDataCon conID bc libraryID projectID = astDataOp bc libraryID projectID $ \data_ -> do
    let cons = filter ((/=) conID . view Expr.id) $ data_ ^. Expr.cons
    return (data_ & Expr.cons .~ cons, ())


modifyDataCon :: Expr -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCon con conID bc libraryID projectID = astDataOp bc libraryID projectID $ \data_ -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedCon, astInfo')  <- EitherT $ IDFixer.runExpr astInfo Nothing True con
    Batch.setASTInfo astInfo' libraryID projectID
    let fixedCon' = fixedCon & Expr.id .~ conID
        cons = filter ((/=) conID . view Expr.id) $ data_ ^. Expr.cons
    return (data_ & Expr.cons .~ (fixedCon':cons), ())


addDataConField :: Expr -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch AST.ID
addDataConField field conID bc libraryID projectID = astDataConOp conID bc libraryID projectID $ \dataCon -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedField, astInfo') <- EitherT $ IDFixer.runExpr astInfo Nothing True field
    Batch.setASTInfo astInfo' libraryID projectID
    return (dataCon & Expr.fields %~ (fixedField:), fixedField ^. Expr.id)


deleteDataConField :: AST.ID -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
deleteDataConField fieldID conID bc libraryID projectID = astDataConOp conID bc libraryID projectID $ \dataCon -> do
    let fields = filter ((/=) fieldID . view Expr.id) $ dataCon ^. Expr.fields
    return (dataCon & Expr.fields .~ fields, ())


modifyDataConField :: Expr -> AST.ID -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataConField field fieldID conID bc libraryID projectID = astDataConOp conID bc libraryID projectID $ \dataCon -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedField, astInfo')  <- EitherT $ IDFixer.runExpr astInfo Nothing True field
    Batch.setASTInfo astInfo' libraryID projectID
    let fixedField' = fixedField & Expr.id .~ fieldID
        (a, b) = List.break ((==) fieldID . view Expr.id) $ dataCon ^. Expr.fields
    b' <- case b of
        _:t -> return $ fixedField':t
        _   -> left $ "No field with id = " ++ show conID
    return (dataCon & Expr.fields .~ (a ++ b') , ())


modifyDataClasses :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataClasses classes bc libraryID projectID = astDataOp bc libraryID projectID $ \data_ -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedClasses, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True classes
    Batch.setASTInfo astInfo' libraryID projectID
    return (data_ & Expr.classes .~ fixedClasses, ())


modifyDataMethods :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataMethods methods bc libraryID projectID = astDataOp bc libraryID projectID $ \data_ -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedMethods, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True methods
    Batch.setASTInfo astInfo' libraryID projectID
    return (data_ & Expr.methods .~ fixedMethods, ())


modifyFunctionName :: Name -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionName name bc libraryID projectID = astFunctionOp bc libraryID projectID $ \fun ->
    return (fun & Expr.fname .~ name, ())


modifyFunctionPath :: [String] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionPath path bc libraryID projectID = astFunctionOp bc libraryID projectID $ \fun ->
    return (fun & Expr.path .~ path, ())


modifyFunctionInputs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionInputs inputs bc libraryID projectID = astFunctionOp bc libraryID projectID $ \fun -> do
    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedInputs, astInfo') <- EitherT $ IDFixer.runExprs astInfo Nothing True inputs
    Batch.setASTInfo astInfo' libraryID projectID
    return (fun & Expr.inputs .~ fixedInputs, ())


modifyFunctionOutput :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionOutput output bc libraryID projectID = do
    function    <- Batch.getFunction bc libraryID projectID
    propertyMap <- Batch.getPropertyMap libraryID projectID

    let oldID = function ^?! Expr.output . Type.id

    astInfo <- Batch.getASTInfo libraryID projectID
    (fixedOutput, astInfo') <- EitherT $ IDFixer.runType astInfo (Just oldID) True output
    Batch.setASTInfo astInfo' libraryID projectID

    let newID = fixedOutput ^. Type.id
        newFunction    = function & Expr.output .~ fixedOutput
        newPropertyMap = PropertyMap.move (-oldID) (-newID) propertyMap

    Batch.setFunctionFocus newFunction bc libraryID projectID
    Batch.setPropertyMap newPropertyMap libraryID projectID
