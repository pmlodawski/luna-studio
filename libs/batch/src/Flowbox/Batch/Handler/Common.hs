---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}

module Flowbox.Batch.Handler.Common where

import           Control.Exception  (IOException)
import qualified Control.Exception  as Exception
import           Control.Monad.RWS
import qualified System.Environment as Environment
import           Text.Show.Pretty

import           Flowbox.Batch.Batch                                       (Batch)
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Process.Map                                 (ProcessMap)
import           Flowbox.Batch.Project.Project                             (Project)
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.ProjectManager                      (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                      as ProjectManager
import qualified Flowbox.Control.Concurrent                                as Concurrent
import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Common                              as AST
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs                   (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Expr                                (Expr)
import           Flowbox.Luna.Data.AST.Module                              (Module)
import           Flowbox.Luna.Data.AST.Zipper.Focus                        (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                        as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                       as Zipper
import           Flowbox.Luna.Data.Graph.Graph                             (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                             as Graph
import           Flowbox.Luna.Data.Graph.Node                              (Node)
import qualified Flowbox.Luna.Data.Graph.Node                              as Node
import           Flowbox.Luna.Data.GraphView.GraphView                     (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView                     as GraphView
import qualified Flowbox.Luna.Data.Pass.ASTInfo                            as ASTInfo
import qualified Flowbox.Luna.Data.Pass.Source                             as Source
import           Flowbox.Luna.Data.PropertyMap                             (PropertyMap)
import qualified Flowbox.Luna.Interpreter.Interpreter                      as Interpreter
import           Flowbox.Luna.Lib.LibManager                               (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                               as LibManager
import           Flowbox.Luna.Lib.Library                                  (Library)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import qualified Flowbox.Luna.Passes.Analysis.Alias.Alias                  as Alias
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID                     as MaxID
import qualified Flowbox.Luna.Passes.Build.Build                           as Build
import qualified Flowbox.Luna.Passes.Build.Diagnostics                     as Diagnostics
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer         as IDFixer
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder       as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser         as GraphParser
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import           Flowbox.Prelude                                           hiding (error)
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Batch.Handler.Common"


safeInterpretLibrary :: Library.ID -> Project.ID -> Batch ()
safeInterpretLibrary libraryID projectID = do
    args <- liftIO Environment.getArgs
    batch  <- get
    unless ("--no-auto-interpreter" `elem` args) $
            liftIO $ Concurrent.forkIO_ $ Exception.catch
                                 (eitherStringToM =<< Batch.runBatch batch (interpretLibrary libraryID projectID))
                                 (\e -> logger error $ "Interpret failed: " ++ show (e :: IOException))


interpretLibrary :: Library.ID -> Project.ID -> Batch ()
interpretLibrary libraryID projectID = do
    let diag    = Diagnostics.all -- TODO [PM] : hardcoded diagnostics
        imports = ["Luna.Target.HS.Core", "Flowbox.Graphics.Mockup", "FlowboxM.Libs.Flowbox.Std"] -- TODO [PM] : hardcoded imports
    ast <- getAST libraryID projectID
    cfg <- gets (view Batch.config)
    maxID <- EitherT $ MaxID.run ast
    [hsc] <- EitherT $ Build.prepareSources diag ast (ASTInfo.mk maxID) False
    let code = unlines $ dropWhile (not . (== "-- body --")) (lines $ Source.code hsc)
    liftIO $ Interpreter.runSource cfg imports code "main"


getProjectManager :: Batch ProjectManager
getProjectManager = gets (view Batch.projectManager)


setProjectManager :: ProjectManager -> Batch ()
setProjectManager projectManager = modify (set Batch.projectManager projectManager)


getProject :: Project.ID -> Batch Project
getProject projectID = do
    projectManager <- getProjectManager
    ProjectManager.lab projectManager projectID <??> ("Wrong 'projectID' = " ++ show projectID)


setProject :: Project -> Project.ID -> Batch ()
setProject newProject projectID = do
    pm <- getProjectManager
    setProjectManager (ProjectManager.updateNode (projectID, newProject) pm)


getProcessMap :: Project.ID -> Batch ProcessMap
getProcessMap projectID = view Project.processMap <$> getProject projectID


setProcessMap :: ProcessMap -> Project.ID -> Batch ()
setProcessMap newProcessMap projectID = do
    project <- getProject projectID
    setProject (project & Project.processMap .~ newProcessMap) projectID


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
    Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc m


setFocus :: Focus -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFocus newFocus bc libraryID projectID = do
    m      <- getAST libraryID projectID
    zipper <- Zipper.focusBreadcrumbs' bc m
    newM   <- Zipper.modify (const newFocus) zipper >>= Zipper.close
    setAST newM libraryID projectID


getModuleFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Module
getModuleFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    case focus of
        Focus.ModuleFocus m -> return m
        _                   -> left "Target is not a module"


setModuleFocus :: Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setModuleFocus newModule = setFocus (Focus.ModuleFocus newModule)


getFunctionFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getFunctionFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    case focus of
        Focus.FunctionFocus f -> return f
        _                     -> left "Target is not a function"


setFunctionFocus :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setFunctionFocus newFunction = setFocus (Focus.FunctionFocus newFunction)


getClassFocus :: Breadcrumbs -> Library.ID -> Project.ID -> Batch Expr
getClassFocus bc libraryID projectID = do
    focus <- getFocus bc libraryID projectID
    case focus of
        Focus.ClassFocus c -> return c
        _                  -> left "Target is not a class"


setClassFocus :: Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setClassFocus newClass = setFocus (Focus.ClassFocus newClass)


getGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (Graph, PropertyMap)
getGraph bc libraryID projectID = do
    ast         <- getAST libraryID projectID
    propertyMap <- getPropertyMap libraryID projectID
    expr        <- getFunctionFocus bc libraryID projectID
    aa          <- EitherT $ Alias.run ast
    EitherT $ GraphBuilder.run aa propertyMap expr


setGraph :: (Graph, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraph (newGraph, newPM) bc libraryID projectID = do
    expr <- getFunctionFocus bc libraryID projectID
    ast  <- EitherT $ GraphParser.run newGraph newPM expr

    newMaxID <- EitherT $ MaxID.runExpr ast
    fixedAst <- EitherT $ IDFixer.runExpr newMaxID Nothing False ast

    logger debug $ show newGraph
    logger debug $ show newPM
    logger debug $ ppShow fixedAst
    setFunctionFocus fixedAst bc libraryID projectID
    setPropertyMap newPM libraryID projectID


getGraphView :: Breadcrumbs -> Library.ID -> Project.ID -> Batch (GraphView, PropertyMap)
getGraphView bc libraryID projectID = do
    (graph, propertyMap) <- getGraph bc libraryID projectID
    let graphView = GraphView.fromGraph graph
    return $ Defaults.removeDefaults graphView propertyMap


setGraphView :: (GraphView, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch ()
setGraphView (newGraphView', newPM') bc libraryID projectID = do
    let (newGraphView, newPM) = Defaults.addDefaults newGraphView' newPM'
    newGraph <- GraphView.toGraph newGraphView
    setGraph (newGraph, newPM) bc libraryID projectID


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


processMapOp :: Project.ID -> (ProcessMap -> Batch (ProcessMap, r)) -> Batch r
processMapOp projectID operation = do
    processMap <- getProcessMap projectID
    (newProcessMap, r) <- operation processMap
    setProcessMap newProcessMap projectID
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
libraryOp libraryID projectID operation = do
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

