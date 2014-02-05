---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Common where

import Control.Monad.RWS
import Text.Show.Pretty

import           Control.Exception                                         (IOException)
import qualified Control.Exception                                         as Exception
import           Flowbox.Batch.Batch                                       (Batch)
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Process.Map                                 (ProcessMap)
import           Flowbox.Batch.Project.Project                             (Project)
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.ProjectManager                      (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                      as ProjectManager
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
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer         as IDFixer
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder       as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser         as GraphParser
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import           Flowbox.Prelude                                           hiding (error)
import           Flowbox.System.Log.Logger
import qualified Control.Concurrent as Concurrent
import qualified System.Environment as Environment


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Common"


safeInterpretLibrary :: Library.ID -> Project.ID -> Batch -> IO ()
safeInterpretLibrary libID projectID batch = do 
    args <- Environment.getArgs
    if "--no-auto-interpreter" `elem` args
        then return ()
        else do _ <- Concurrent.forkIO $ Exception.catch
                                 (interpretLibrary libID projectID batch)
                                 (\e -> loggerIO error $ "Interpret failed: " ++ show (e :: IOException))
                return ()


interpretLibrary :: Library.ID -> Project.ID -> Batch -> IO ()
interpretLibrary libID projectID batch = do
    ast <- getAST libID projectID batch
    let diag    = Diagnostics.all -- TODO [PM] : hardcoded diagnostics
        cfg     = Batch.config batch
        imports = ["Luna.Target.HS.Core", "Flowbox.Graphics.Mockup", "FlowboxM.Libs.Flowbox.Std"] -- TODO [PM] : hardcoded imports
    maxID <- Luna.runIO $ MaxID.run ast
    [hsc] <- Luna.runIO $ Build.prepareSources diag ast (ASTInfo.mk maxID) False
    let code = unlines $ snd $ break (=="-- body --") $ lines $ Source.code hsc
    Interpreter.runSource cfg imports code "main"


getProjectManager :: Batch -> ProjectManager
getProjectManager = Batch.projectManager


setProjectManager :: ProjectManager -> Batch -> Batch
setProjectManager newProjectManager batch = batch { Batch.projectManager = newProjectManager }


getProject :: (Applicative m, Monad m) => Project.ID -> Batch -> m Project
getProject projectID batch  =
    ProjectManager.lab (getProjectManager batch) projectID <?> ("Wrong 'projectID' = " ++ show projectID)


setProject :: Project -> Project.ID -> Batch -> Batch
setProject newProject projectID batch = newBatch where
    projectManager    = getProjectManager batch
    newProjectManager = ProjectManager.updateNode (projectID, newProject) projectManager
    newBatch          = setProjectManager newProjectManager batch


getProcessMap :: (Applicative m, Monad m) => Project.ID -> Batch -> m ProcessMap
getProcessMap projectID batch = Project.processMap <$> getProject projectID batch


setProcessMap :: (Applicative m, Monad m) => ProcessMap -> Project.ID -> Batch -> m Batch
setProcessMap newProcessMap projectID batch = do
    project <- getProject projectID batch
    let newProject = project { Project.processMap = newProcessMap }
    return $ setProject newProject projectID batch


getLibManager :: (Applicative m, Monad m) => Project.ID -> Batch -> m LibManager
getLibManager projectID batch = Project.libs <$> getProject projectID batch


setLibManager :: (Applicative m, Monad m) => LibManager -> Project.ID -> Batch -> m Batch
setLibManager newLibManager projectID batch = do
    project <- getProject projectID batch
    let newProject = project { Project.libs = newLibManager }
    return $ setProject newProject projectID batch


getLibrary :: (Applicative m, Monad m) => Library.ID -> Project.ID -> Batch -> m Library
getLibrary libraryID projectID batch = do
    libManager <- getLibManager projectID batch
    LibManager.lab libManager libraryID <?> ("Wrong 'libraryID' = " ++ show libraryID)


setLibrary :: (Applicative m, Monad m) => Library -> Library.ID -> Project.ID -> Batch -> m Batch
setLibrary newLibary libraryID projectID batch = do
    libManager <- getLibManager projectID batch
    let newLibManager = LibManager.updateNode (libraryID, newLibary) libManager
    setLibManager newLibManager projectID batch


getPropertyMap :: (Applicative m, Monad m) => Library.ID -> Project.ID -> Batch -> m PropertyMap
getPropertyMap libraryID projectID batch =
    Library.propertyMap <$> getLibrary libraryID projectID batch


setPropertyMap :: (Applicative m, Monad m) => PropertyMap -> Library.ID -> Project.ID -> Batch -> m Batch
setPropertyMap newPropertyMap libraryID projectID batch = do
    library <- getLibrary libraryID projectID batch
    let newLibrary = library { Library.propertyMap = newPropertyMap }
    setLibrary newLibrary libraryID projectID batch


getAST :: (Applicative m, Monad m) => Library.ID -> Project.ID -> Batch -> m Module
getAST libraryID projectID batch =
    Library.ast <$> getLibrary libraryID projectID batch


setAST :: (Applicative m, Monad m) => Module -> Library.ID -> Project.ID -> Batch -> m Batch
setAST newModule libraryID projectID batch = do
    library <- getLibrary libraryID projectID batch
    let newLibrary = library { Library.ast = newModule }
    setLibrary newLibrary libraryID projectID batch


getFocus :: (Applicative m, Monad m) => Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Focus
getFocus bc libraryID projectID batch = do
    m <- getAST libraryID projectID batch
    Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc m


setFocus :: (Applicative m, Monad m) => Focus -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
setFocus newFocus bc libraryID projectID batch = do
    m      <- getAST libraryID projectID batch
    zipper <- Zipper.focusBreadcrumbs' bc m
    newM   <- Zipper.modify (\_ -> newFocus) zipper >>= Zipper.close
    setAST newM libraryID projectID batch


getModuleFocus :: (Applicative m, Monad m) => Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Module
getModuleFocus bc libraryID projectID batch = do
    focus <- getFocus bc libraryID projectID batch
    case focus of
        Focus.ModuleFocus m -> return m
        _                   -> fail "Target is not a module"


setModuleFocus :: (Applicative m, Monad m) => Module -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
setModuleFocus newModule bc libraryID projectID batch =
    setFocus (Focus.ModuleFocus newModule) bc libraryID projectID batch



getFunctionFocus :: (Applicative m, Monad m) => Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Expr
getFunctionFocus bc libraryID projectID batch = do
    focus <- getFocus bc libraryID projectID batch
    case focus of
        Focus.FunctionFocus f -> return f
        _                     -> fail "Target is not a function"


setFunctionFocus :: (Applicative m, Monad m) => Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
setFunctionFocus newFunction bc libraryID projectID batch =
    setFocus (Focus.FunctionFocus newFunction) bc libraryID projectID batch


getClassFocus :: (Applicative m, Monad m) => Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Expr
getClassFocus bc libraryID projectID batch = do
    focus <- getFocus bc libraryID projectID batch
    case focus of
        Focus.ClassFocus c -> return c
        _                  -> fail "Target is not a class"


setClassFocus :: (Applicative m, Monad m) => Expr -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> m Batch
setClassFocus newClass bc libraryID projectID batch =
    setFocus (Focus.ClassFocus newClass) bc libraryID projectID batch


getGraph :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (Graph, PropertyMap)
getGraph bc libraryID projectID batch = do
    ast         <- getAST libraryID projectID batch
    propertyMap <- getPropertyMap libraryID projectID batch
    expr        <- getFunctionFocus bc libraryID projectID batch
    aa          <- Luna.runIO $ Alias.run ast
    Luna.runIO $ GraphBuilder.run aa propertyMap expr


setGraph :: (Graph, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
setGraph (newGraph, newPM) bc libraryID projectID batch = do
    expr <- getFunctionFocus bc libraryID projectID batch
    ast  <- Luna.runIO $ GraphParser.run newGraph newPM expr

    newMaxID <- Luna.runIO $ MaxID.runExpr ast
    fixedAst <- Luna.runIO $ IDFixer.runExpr newMaxID False ast

    loggerIO debug $ show newGraph
    loggerIO debug $ show newPM
    loggerIO debug $ ppShow fixedAst
    batch <- setFunctionFocus fixedAst bc libraryID projectID batch
    setPropertyMap newPM libraryID projectID batch


getGraphView :: Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO (GraphView, PropertyMap)
getGraphView bc libraryID projectID batch = do
    (graph, propertyMap) <- getGraph bc libraryID projectID batch
    let graphView = GraphView.fromGraph graph
    return $ Defaults.removeDefaults graphView propertyMap


setGraphView :: (GraphView, PropertyMap) -> Breadcrumbs -> Library.ID -> Project.ID -> Batch -> IO Batch
setGraphView (newGraphView', newPM') bc libraryID projectID batch = do
    let (newGraphView, newPM) = Defaults.addDefaults newGraphView' newPM'
    newGraph <- GraphView.toGraph newGraphView
    setGraph (newGraph, newPM) bc libraryID projectID batch

-- DEPRECATED ---------------------------------------------------------------

readonly :: (Applicative m, Monad m) => m (a, r) -> m r
readonly operation = snd <$> operation


noresult :: (Applicative m, Monad m) => m (a, r) -> m a
noresult operation = fst <$> operation


projectManagerOp :: (Applicative m, Monad m)
                 => (Batch -> ProjectManager -> m (ProjectManager, r))
                 -> Batch
                 -> m (Batch, r)
projectManagerOp operation batch = do
    let aprojectManager = Batch.projectManager batch
    (newProjectManager, r) <- operation batch aprojectManager
    let newBatch = batch { Batch.projectManager = newProjectManager }
    return (newBatch, r)


projectOp :: (Applicative m, Monad m)
          => Project.ID
          -> (Batch -> Project -> m (Project, r))
          -> Batch
          -> m (Batch, r)
projectOp projectID operation = projectManagerOp (\batch aprojectManager -> do
    project         <- ProjectManager.lab aprojectManager projectID <?> ("Wrong 'projectID' = " ++ show projectID)
    (newProject, r) <- operation batch project
    let newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
    return (newProjectManager, r))


processMapOp :: (Applicative m, Monad m)
             => Project.ID
             -> (Batch -> ProcessMap -> m (ProcessMap, r))
             -> Batch
             -> m (Batch, r)
processMapOp projectID operation = projectOp projectID (\batch project -> do
    let processMap = Project.processMap $ project
    (newProcessMap, r) <- operation batch processMap
    let newProject = project { Project.processMap = newProcessMap }
    return (newProject, r))


libManagerOp :: (Applicative m, Monad m)
             => Project.ID
             -> (Batch -> LibManager -> m (LibManager, r))
             -> Batch
             -> m (Batch, r)
libManagerOp projectID operation = projectOp projectID (\batch project -> do
    let libManager = Project.libs $ project
    (newLibManager, r) <- operation batch libManager
    let newProject = project { Project.libs = newLibManager }
    return (newProject, r))


libraryOp :: (Applicative m, Monad m)
          => Library.ID
          -> Project.ID
          -> (Batch -> Library -> m (Library, r))
          -> Batch
          -> m (Batch, r)
libraryOp libID projectID operation = libManagerOp projectID (\batch libManager -> do
    library        <- LibManager.lab libManager libID <?> ("Wrong 'libID' = " ++ show libID)
    (newLibary, r) <- operation batch library
    let newLibManager = LibManager.updateNode (libID, newLibary) libManager
    return (newLibManager, r))


astOp :: Library.ID
      -> Project.ID
      -> (Batch -> Module -> PropertyMap -> IO ((Module, PropertyMap), r))
      -> Batch
      -> IO (Batch, r)
astOp libID projectID operation = libraryOp libID projectID (\batch library -> do
    let ast         = Library.ast library
        propertyMap = Library.propertyMap library
    ((newAst, newPM), r) <- operation batch ast propertyMap
    let newLibrary = library { Library.ast = newAst, Library.propertyMap = newPM }
    return (newLibrary, r))


astFocusOp :: Breadcrumbs
           -> Library.ID
           -> Project.ID
           -> (Batch -> Focus -> AST.ID -> IO (Focus, r))
           -> Batch
           -> IO (Batch, r)
astFocusOp bc libID projectID operation = astOp libID projectID (\batch ast pm -> do
    zipper <- Zipper.focusBreadcrumbs' bc ast
    let focus = Zipper.getFocus zipper

    maxID <- Luna.runIO $ MaxID.run ast
    (newFocus, r) <- operation batch focus maxID

    newAst <- Zipper.modify (\_ -> newFocus) zipper >>= Zipper.close
    return ((newAst, pm), r))


astModuleFocusOp :: Breadcrumbs
                 -> Library.ID
                 -> Project.ID
                 -> (Batch -> Module -> AST.ID -> IO (Module, r))
                 -> Batch
                 -> IO (Batch, r)
astModuleFocusOp bc libID projectID operation = astFocusOp bc libID projectID (\batch focus maxID -> do
    (m, r) <- case focus of
        Focus.ModuleFocus m -> operation batch m maxID
        _                   -> fail "Target is not a module"
    return (Focus.ModuleFocus m, r))


astFunctionFocusOp :: Breadcrumbs
                   -> Library.ID
                   -> Project.ID
                   -> (Batch -> Expr -> AST.ID -> IO (Expr, r))
                   -> Batch
                   -> IO (Batch, r)
astFunctionFocusOp bc libID projectID operation = astFocusOp bc libID projectID (\batch focus maxID -> do
    (f, r) <- case focus of
        Focus.FunctionFocus f -> operation batch f maxID
        _                     -> fail "Target is not a function"
    return (Focus.FunctionFocus f, r))


astClassFocusOp :: Breadcrumbs
                -> Library.ID
                -> Project.ID
                -> (Batch -> Expr -> AST.ID -> IO (Expr, r))
                -> Batch
                -> IO (Batch, r)
astClassFocusOp bc libID projectID operation = astFocusOp bc libID projectID (\batch focus maxID -> do
    (c, r) <- case focus of
        Focus.ClassFocus c -> operation batch c maxID
        _                  -> fail "Target is not a class"
    return (Focus.ClassFocus c, r))


graphOp :: Breadcrumbs
         -> Library.ID
         -> Project.ID
         -> (Batch -> Graph -> PropertyMap -> AST.ID -> IO ((Graph, PropertyMap), r))
         -> Batch
         -> IO (Batch, r)
graphOp bc libID projectID operation = astOp libID projectID (\batch ast propertyMap -> do
    zipper <- Zipper.focusBreadcrumbs' bc ast
    let focus = Zipper.getFocus zipper
    expr <- case focus of
        Focus.FunctionFocus expr -> return expr
        _                        -> fail "Breadcrumbs are not focusing on function."
    aa    <- Luna.runIO $ Alias.run ast
    maxID <- Luna.runIO $ MaxID.run ast

    (graph, pm) <- Luna.runIO $ GraphBuilder.run aa propertyMap expr

    ((newGraph, newPM), r) <- liftIO $ operation batch graph pm maxID

    ast' <- Luna.runIO $ GraphParser.run newGraph newPM expr

    newMaxID <- Luna.runIO $ MaxID.runExpr ast'
    fixedAst <- Luna.runIO $ IDFixer.runExpr newMaxID False ast'

    loggerIO debug $ show newGraph
    loggerIO debug $ show newPM
    loggerIO debug $ ppShow fixedAst

    newAst <- Zipper.modify (\_ -> Focus.FunctionFocus fixedAst) zipper >>= Zipper.close
    return ((newAst, newPM), r))


graphViewOp :: Breadcrumbs
            -> Library.ID
            -> Project.ID
            -> (Batch -> GraphView -> PropertyMap -> AST.ID -> IO ((GraphView, PropertyMap), r))
            -> Batch
            -> IO (Batch, r)
graphViewOp bc libID projectID operation = graphOp bc libID projectID (\batch graph propertyMap maxID -> do
    let graphView = GraphView.fromGraph graph
    let (graphView', propertyMap') = Defaults.removeDefaults graphView propertyMap
    ((newGraphView', newPM'), r) <- operation batch graphView' propertyMap' maxID
    let (newGraphView, newPM) = Defaults.addDefaults newGraphView' newPM'
    newGraph <- GraphView.toGraph newGraphView
    return ((newGraph, newPM), r))


readonlyNodeOp :: Node.ID
               -> Breadcrumbs
               -> Library.ID
               -> Project.ID
               -> (Batch -> Node -> IO r)
               -> Batch
               -> IO r
readonlyNodeOp nodeID bc libID projectID operation = readonly . graphViewOp bc libID projectID (\batch graph propertyMap _ -> do
    node <- Graph.lab graph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    r <- operation batch node
    return ((graph, propertyMap), r))
