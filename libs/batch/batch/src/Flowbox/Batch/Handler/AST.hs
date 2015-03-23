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
        Focus.Lambda   _ -> left "Cannot add module to a lambda"
        Focus.Module   m -> return $ Focus.Module $ Module.addModule fixedModule m
    return (newFocus, fixedModule))


addData :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addData newClass bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedClass <- EitherT $ IDFixer.runExpr maxID Nothing True newClass
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addClass fixedClass c
        Focus.Function _ -> left "Cannot add class to a function"
        Focus.Lambda   _ -> left "Cannot add class to a lambda"
        Focus.Module   m -> return $ Focus.Module $ Module.addClass fixedClass m
    return (newFocus, fixedClass))


addFunction :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
addFunction newFunction bcParent libID projectID = astFocusOp bcParent libID projectID (\focus -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedFunction <- EitherT $ IDFixer.runExpr maxID Nothing True newFunction
    newFocus <- case focus of
        Focus.Class    c -> return $ Focus.Class $ Expr.addMethod fixedFunction c
        Focus.Function _ -> left "Cannot add function to a function"
        Focus.Lambda   _ -> left "Cannot add function to a lambda"
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
resolveDefinition name bc libID projectID = do
    libManager <- Batch.getLibManager projectID
    results <- EitherT $ NameResolver.run name bc libID libManager
    return (map Tuple.swap results)


modifyModuleCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleCls cls bc libID projectID = astModuleOp bc libID projectID $ \m -> do
    maxID    <- Batch.getMaxID libID projectID
    fixedCls <- EitherT $ IDFixer.runType maxID (Just $ m ^. Module.cls . Type.id) True cls
    return (m & Module.cls .~ fixedCls, ())


modifyModuleImports :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleImports imports bc libID projectID = astModuleOp bc libID projectID $ \m -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedImports <- EitherT $ IDFixer.runExprs maxID Nothing True imports
    return (m & Module.imports .~ fixedImports, ())


modifyModuleTypeAliases :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleTypeAliases typeAliases bc libID projectID = astModuleOp bc libID projectID $ \m -> do
    maxID <- Batch.getMaxID libID projectID
    fixedTypeAliases <- EitherT $ IDFixer.runExprs maxID Nothing True typeAliases
    return (m & Module.typeAliases .~ fixedTypeAliases, ())


modifyModuleTypeDefs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleTypeDefs typeDefs bc libID projectID = astModuleOp bc libID projectID $ \m -> do
    maxID <- Batch.getMaxID libID projectID
    fixedTypeDefs <- EitherT $ IDFixer.runExprs maxID Nothing True typeDefs
    return (m & Module.typeDefs .~ fixedTypeDefs, ())


modifyModuleFields :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyModuleFields fields bc libID projectID = astModuleOp bc libID projectID $ \m -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedFields <- EitherT $ IDFixer.runExprs maxID Nothing True fields
    return (m & Module.fields .~ fixedFields, ())


modifyDataCls :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCls cls bc libID projectID = astDataOp bc libID projectID (\m -> do
    maxID    <- Batch.getMaxID libID projectID
    fixedCls <- EitherT $ IDFixer.runType maxID (Just $ m ^?! Expr.cls . Type.id) True cls
    return (m & Expr.cls .~ fixedCls, ()))


modifyDataCons :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCons cons bc libID projectID = astDataOp bc libID projectID $ \m -> do
    maxID     <- Batch.getMaxID libID projectID
    fixedCons <- EitherT $ IDFixer.runExprs maxID Nothing True cons
    return (m & Expr.cons .~ fixedCons, ())


addDataCon :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
addDataCon con bc libID projectID = astDataOp bc libID projectID $ \data_ -> do
    maxID     <- Batch.getMaxID libID projectID
    fixedCon  <- EitherT $ IDFixer.runExpr maxID Nothing True con
    return (data_ & Expr.cons %~ (fixedCon:), ())


deleteDataCon :: AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
deleteDataCon conID bc libID projectID = astDataOp bc libID projectID $ \data_ -> do
    let cons = filter ((/=) conID . view Expr.id) $ data_ ^. Expr.cons
    return (data_ & Expr.cons .~ cons, ())


modifyDataCon :: Expr -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataCon con conID bc libID projectID = astDataOp bc libID projectID $ \data_ -> do
    maxID     <- Batch.getMaxID libID projectID
    fixedCon  <- EitherT $ IDFixer.runExpr maxID Nothing True con
    let fixedCon' = fixedCon & Expr.id .~ conID
        cons = filter ((/=) conID . view Expr.id) $ data_ ^. Expr.cons
    return (data_ & Expr.cons .~ (fixedCon':cons), ())


addDataConField :: Expr -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
addDataConField field conID bc libID projectID = astDataConOp conID bc libID projectID $ \dataCon -> do
    maxID      <- Batch.getMaxID libID projectID
    fixedField <- EitherT $ IDFixer.runExpr maxID Nothing True field
    return (dataCon & Expr.fields %~ (fixedField:), ())


deleteDataConField :: AST.ID -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
deleteDataConField fieldID conID bc libID projectID = astDataConOp conID bc libID projectID $ \dataCon -> do
    let fields = filter ((/=) fieldID . view Expr.id) $ dataCon ^. Expr.fields
    return (dataCon & Expr.fields .~ fields, ())


modifyDataConField :: Expr -> AST.ID -> AST.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataConField field fieldID conID bc libID projectID = astDataConOp conID bc libID projectID $ \dataCon -> do
    maxID     <- Batch.getMaxID libID projectID
    fixedField  <- EitherT $ IDFixer.runExpr maxID Nothing True field
    let fixedField' = fixedField & Expr.id .~ fieldID
        (a, b) = List.break ((==) fieldID . view Expr.id) $ dataCon ^. Expr.fields
    b' <- case b of
        _:t -> return $ fixedField':t
        _   -> left $ "No field with id = " ++ show conID
    return (dataCon & Expr.fields .~ (a ++ b') , ())


modifyDataClasses :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataClasses classes bc libID projectID = astDataOp bc libID projectID $ \data_ -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedClasses <- EitherT $ IDFixer.runExprs maxID Nothing True classes
    return (data_ & Expr.classes .~ fixedClasses, ())


modifyDataMethods :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyDataMethods methods bc libID projectID = astDataOp bc libID projectID $ \data_ -> do
    maxID        <- Batch.getMaxID libID projectID
    fixedMethods <- EitherT $ IDFixer.runExprs maxID Nothing True methods
    return (data_ & Expr.methods .~ fixedMethods, ())


modifyFunctionName :: Name -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionName name bc libID projectID = astFunctionOp bc libID projectID $ \fun ->
    return (fun & Expr.fname .~ name, ())


modifyFunctionPath :: [String] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionPath path bc libID projectID = astFunctionOp bc libID projectID $ \fun ->
    return (fun & Expr.path .~ path, ())


modifyFunctionInputs :: [Expr] -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionInputs inputs bc libID projectID = astFunctionOp bc libID projectID $ \fun -> do
    maxID       <- Batch.getMaxID libID projectID
    fixedInputs <- EitherT $ IDFixer.runExprs maxID Nothing True inputs
    return (fun & Expr.inputs .~ fixedInputs, ())


modifyFunctionOutput :: Type -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
modifyFunctionOutput output bc libID projectID = do
    function    <- Batch.getFunction bc libID projectID
    propertyMap <- Batch.getPropertyMap libID projectID

    let oldID = function ^?! Expr.output . Type.id

    maxID       <- Batch.getMaxID libID projectID
    fixedOutput <- EitherT $ IDFixer.runType maxID (Just oldID) True output

    let newID = fixedOutput ^. Type.id
        newFunction    = function & Expr.output .~ fixedOutput
        newPropertyMap = PropertyMap.move (-oldID) (-newID) propertyMap

    Batch.setFunctionFocus newFunction bc libID projectID
    Batch.setPropertyMap newPropertyMap libID projectID
