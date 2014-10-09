---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Batch.Handler.Common where

import           Control.Exception  (IOException)
import qualified Control.Exception  as Exception
import           Control.Monad.RWS
import           Data.Int           (Int32)
import qualified System.Environment as Environment
import           Text.Show.Pretty

import           Flowbox.Batch.Batch                       (Batch)
import qualified Flowbox.Batch.Batch                       as Batch
import           Flowbox.Batch.Project.Project             (Project)
import qualified Flowbox.Batch.Project.Project             as Project
import           Flowbox.Batch.Project.ProjectManager      (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager      as ProjectManager
import qualified Flowbox.Control.Concurrent                as Concurrent
import           Flowbox.Control.Error
import           Flowbox.Prelude                           hiding (error)
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common                           as AST
import           Luna.AST.Control.Crumb                    (Breadcrumbs)
import           Luna.AST.Control.Focus                    (Focus)
import qualified Luna.AST.Control.Focus                    as Focus
import qualified Luna.AST.Control.Zipper                   as Zipper
import           Luna.AST.Expr                             (Expr)
import           Luna.AST.Module                           (Module)
import qualified Luna.Data.ASTInfo                         as ASTInfo
import qualified Luna.Data.Source                          as Source
import           Luna.Graph.Graph                          (Graph)
import qualified Luna.Graph.Graph                          as Graph
import           Luna.Graph.Node                           (Node)
import qualified Luna.Graph.Node                           as Node
import           Luna.Graph.PropertyMap                    (PropertyMap)
import           Luna.Graph.View.GraphView                 (GraphView)
import qualified Luna.Graph.View.GraphView                 as GraphView
import qualified Luna.Interpreter                          as Interpreter
import           Luna.Lib.Lib                              (Library)
import qualified Luna.Lib.Lib                              as Library
import           Luna.Lib.Manager                          (LibManager)
import qualified Luna.Lib.Manager                          as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias            as Alias
import qualified Luna.Pass.Analysis.ID.MaxID               as MaxID
import qualified Luna.Pass.Build.Build                     as Build
import qualified Luna.Pass.Build.Diagnostics               as Diagnostics
import qualified Luna.Pass.Transform.AST.IDFixer.IDFixer   as IDFixer
import qualified Luna.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Luna.Pass.Transform.Graph.Parser.Parser   as GraphParser
import qualified Luna.Pass.Transform.GraphView.Defaults    as Defaults
import qualified Luna.Pass.Transform.Graph.GCNodeProperties.GCNodeProperties as GCNodeProperties



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


safeInterpretLibrary :: Library.ID -> Project.ID -> Batch ()
safeInterpretLibrary libraryID projectID = do
    args <- liftIO Environment.getArgs
    batch  <- get
    when ("--auto-interpreter" `elem` args) $
            liftIO $ Concurrent.forkIO_ $ Exception.catch
                                 (eitherStringToM =<< Batch.runBatch batch (interpretLibrary libraryID projectID))
                                 (\e -> logger error $ "Interpret failed: " ++ show (e :: IOException))


interpretLibrary :: Library.ID -> Project.ID -> Batch ()
interpretLibrary libraryID projectID = do
    let diag    = Diagnostics.all -- TODO [PM] : hardcoded diagnostics
        imports = ["Luna.Target.HS", "FlowboxM.Libs.Flowbox.Std"] -- TODO [PM] : hardcoded imports
    ast <- getAST libraryID projectID
    cfg <- gets (view Batch.config)
    maxID <- EitherT $ MaxID.run ast
    [hsc] <- EitherT $ Build.prepareSources diag ast (ASTInfo.mk maxID) False
    let code = unlines $ dropWhile (not . (== "-- body --")) (lines $ hsc ^. Source.code)
    liftIO $ Interpreter.runSource cfg imports code "main"


increaseUpdateNo :: Batch ()
increaseUpdateNo = modify (Batch.updateNo %~ (+1))


getUpdateNo :: Batch Int32
getUpdateNo = gets (view Batch.updateNo)


setUpdateNo :: Int32 -> Batch ()
setUpdateNo updateNo = modify (set Batch.updateNo updateNo)


getProjectManager :: Batch ProjectManager
getProjectManager = gets (view Batch.projectManager)


setProjectManager :: ProjectManager -> Batch ()
setProjectManager projectManager =
    increaseUpdateNo >> modify (set Batch.projectManager projectManager)


getProject :: Project.ID -> Batch Project
getProject projectID = do
    projectManager <- getProjectManager
    ProjectManager.lab projectManager projectID <??> ("Wrong 'projectID' = " ++ show projectID)


setProject :: Project -> Project.ID -> Batch ()
setProject newProject projectID = do
    pm <- getProjectManager
    setProjectManager (ProjectManager.updateNode (projectID, newProject) pm)


getLibManager :: Project.ID -> Batch LibManager
getLibManager projectID = view Project.libs <$> getProject projectID


setLibManager :: LibManager -> Project.ID -> Batch ()
setLibManager newLibManager projectID = do
    project <- getProject projectID
    setProject (project & Project.libs .~ newLibManager) projectID


getLibrary :: Library.ID -> Project.ID -> Batch Library
getLibrary libraryID projectID = do
    libManager <- getLibManager projectID
    LibManager.lab libManager libraryID <??> ("Wrong 'libraryID' = " ++ show libraryID)


setLibrary :: Library -> Library.ID -> Project.ID -> Batch ()
setLibrary newLibary libraryID projectID = do
    libManager <- getLibManager projectID
    setLibManager (LibManager.updateNode (libraryID, newLibary) libManager) projectID


getPropertyMap :: Library.ID -> Project.ID -> Batch PropertyMap
getPropertyMap libraryID projectID =
    view Library.propertyMap <$> getLibrary libraryID projectID


setPropertyMap :: PropertyMap -> Library.ID -> Project.ID -> Batch ()
setPropertyMap newPropertyMap libraryID projectID = do
    library <- getLibrary libraryID projectID
    setLibrary (library & Library.propertyMap .~ newPropertyMap) libraryID projectID


getAST :: Library.ID -> Project.ID -> Batch  Module
getAST libraryID projectID =
    view Library.ast <$> getLibrary libraryID projectID


setAST :: Module -> Library.ID -> Project.ID -> Batch ()
setAST newModule libraryID projectID = do
    library <- getLibrary libraryID projectID
    setLibrary (library & Library.ast .~ newModule) libraryID projectID


getFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Focus
getFocus bc libraryID projectID = do
    m <- getAST libraryID projectID
    hoistEither $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc m


setFocus :: Focus -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFocus newFocus bc libraryID projectID = do
    m      <- getAST libraryID projectID
    zipper <- hoistEither $ Zipper.focusBreadcrumbs' bc m
    let newM = Zipper.close $ Zipper.modify (const newFocus) zipper
    setAST newM libraryID projectID


getModuleFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Module
getModuleFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getModule focus <??> "Target is not a module"


setModuleFocus :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setModuleFocus newModule = setFocus (Focus.Module newModule)


getFunctionFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getFunctionFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getFunction focus <??> "Target is not a function"


setFunctionFocus :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFunctionFocus newFunction = setFocus (Focus.Function newFunction)


getClassFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getClassFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    Focus.getClass focus <??> "Target is not a class"


setClassFocus :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setClassFocus newClass = setFocus (Focus.Class newClass)


getGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (Graph, PropertyMap)
getGraph bc libraryID projectID = do
    ast         <- getAST libraryID projectID
    logger trace $ ppShow ast
    propertyMap <- getPropertyMap libraryID projectID
    expr        <- getFunctionFocus bc libraryID projectID
    aa          <- EitherT $ Alias.run ast
    result <- EitherT $ GraphBuilder.run aa propertyMap True expr
    logger trace $ ppShow result
    return result


setGraph :: (Graph, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraph (newGraph, newPM) bc libraryID projectID = do
    logger trace  $ ppShow newGraph
    logger trace $ ppShow newPM
    expr <- getFunctionFocus bc libraryID projectID
    (ast, newPM2)  <- EitherT $ GraphParser.run newGraph newPM expr

    newMaxID <- EitherT $ MaxID.runExpr ast
    fixedAst <- EitherT $ IDFixer.runExpr newMaxID Nothing False ast
    logger debug $ show newGraph
    logger debug $ show newPM2
    logger debug $ ppShow fixedAst
    setFunctionFocus fixedAst bc libraryID projectID
    setPropertyMap newPM2 libraryID projectID
    gcPropertyMap libraryID projectID


gcPropertyMap :: Library.ID -> Project.ID -> Batch ()
gcPropertyMap libraryID projectID = do
    ast <- getAST         libraryID projectID
    pm  <- getPropertyMap libraryID projectID
    fixedPM  <- EitherT $ GCNodeProperties.run ast pm
    setPropertyMap fixedPM libraryID projectID


getGraphView :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (GraphView, PropertyMap)
getGraphView bc libraryID projectID = do
    (graph, propertyMap) <- getGraph bc libraryID projectID
    let (graphView, propertyMap2) = GraphView.fromGraph graph propertyMap
    return $ Defaults.removeDefaults graphView propertyMap2


setGraphView :: (GraphView, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraphView (newGraphView', newPM') bc libraryID projectID = do
    let (newGraphView, newPM) = Defaults.addDefaults newGraphView' newPM'
    graphWithPm <- hoistEither $ GraphView.toGraph newGraphView newPM
    setGraph graphWithPm bc libraryID projectID


getNode :: Node.ID -> Breadcrumbs -> Library.ID -> Project.ID -> Batch Node
getNode nodeID bc libraryID projectID = do
    (graphView, _) <- getGraphView bc libraryID projectID
    Graph.lab graphView nodeID <??> ("Wrong 'nodeID' = " ++ show nodeID)

---------------------------------------------------------------------------

getMaxID :: Library.ID -> Project.ID -> Batch AST.ID
getMaxID libraryID projectID = do
    ast <- getAST libraryID projectID
    EitherT $ MaxID.run ast

---------------------------------------------------------------------------

projectManagerOp :: (ProjectManager -> Batch (ProjectManager, r)) -> Batch r
projectManagerOp operation = do
    projectManager <- getProjectManager
    (newProjectManager, r) <- operation projectManager
    setProjectManager newProjectManager
    return r


projectOp :: Project.ID -> (Project -> Batch (Project, r)) -> Batch r
projectOp projectID operation = do
    project         <- getProject projectID
    (newProject, r) <- operation project
    setProject newProject projectID
    return r


libManagerOp :: Project.ID -> (LibManager -> Batch (LibManager, r)) -> Batch r
libManagerOp projectID operation = do
    libManager <- getLibManager projectID
    (newLibManager, r) <- operation libManager
    setLibManager newLibManager projectID
    return r


libraryOp :: Library.ID -> Project.ID
          -> (Library -> Batch (Library, r))
          -> Batch r
libraryOp libraryID projectID operation =  do
    library        <- getLibrary libraryID projectID
    (newLibary, r) <- operation library
    setLibrary newLibary libraryID projectID
    return r


propertyMapOp :: Library.ID -> Project.ID
              -> (PropertyMap -> Batch (PropertyMap, r))
              -> Batch r
propertyMapOp libraryID projectID operation = do
    propertyMap <- getPropertyMap libraryID projectID
    (newPropertyMap, r) <- operation propertyMap
    setPropertyMap newPropertyMap libraryID projectID
    return r


astOp :: Library.ID -> Project.ID
      -> (Module -> PropertyMap -> Batch ((Module, PropertyMap), r))
      -> Batch r
astOp libraryID projectID operation = do
    ast         <- getAST         libraryID projectID
    propertyMap <- getPropertyMap libraryID projectID
    ((newAst, newPM), r) <- operation ast propertyMap
    setAST         newAst libraryID projectID
    setPropertyMap newPM  libraryID projectID
    return r


astFocusOp :: Breadcrumbs -> Library.ID -> Project.ID
           -> (Focus -> Batch (Focus, r))
           -> Batch r
astFocusOp bc libraryID projectID operation = do
    focus <- getFocus bc libraryID projectID
    (newFocus, r) <- operation focus
    setFocus newFocus bc libraryID projectID
    return r


astModuleFocusOp :: Breadcrumbs -> Library.ID -> Project.ID
                 -> (Module -> Batch (Module, r))
                 -> Batch r
astModuleFocusOp bc libraryID projectID operation = do
    m <- getModuleFocus bc libraryID projectID
    (newM, r) <- operation m
    setModuleFocus newM bc libraryID projectID
    return r


astFunctionFocusOp :: Breadcrumbs -> Library.ID -> Project.ID
                   -> (Expr -> Batch (Expr, r))
                   -> Batch r
astFunctionFocusOp bc libraryID projectID operation = do
    f <- getFunctionFocus bc libraryID projectID
    (newF, r) <- operation f
    setFunctionFocus newF bc libraryID projectID
    return r


astClassFocusOp :: Breadcrumbs -> Library.ID -> Project.ID
                -> (Expr -> Batch (Expr, r))
                -> Batch r
astClassFocusOp bc libraryID projectID operation = do
    c <- getClassFocus bc libraryID projectID
    (newC, r) <- operation c
    setClassFocus newC bc libraryID projectID
    return r


graphOp :: Breadcrumbs -> Library.ID -> Project.ID
        -> (Graph -> PropertyMap -> Batch ((Graph, PropertyMap), r))
        -> Batch r
graphOp bc libraryID projectID operation = do
    (graph, pm)            <- getGraph bc libraryID projectID
    ((newGraph, newPM), r) <- operation graph pm
    setGraph (newGraph, newPM) bc libraryID projectID
    return r


graphViewOp :: Breadcrumbs -> Library.ID -> Project.ID
            -> (GraphView -> PropertyMap -> Batch ((GraphView, PropertyMap), r))
            -> Batch r
graphViewOp bc libraryID projectID operation = do
    (graphView, propertyMap) <- getGraphView bc libraryID projectID
    ((newGraphView, newPM), r) <- operation graphView propertyMap
    setGraphView (newGraphView, newPM) bc libraryID projectID
    return r

