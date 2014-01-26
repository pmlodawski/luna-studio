---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Common where

import Control.Monad.RWS
import Text.Show.Pretty

import           Flowbox.Batch.Batch                                       (Batch)
import qualified Flowbox.Batch.Batch                                       as Batch
import           Flowbox.Batch.Process.Map                                 (ProcessMap)
import           Flowbox.Batch.Project.Project                             (Project)
import qualified Flowbox.Batch.Project.Project                             as Project
import           Flowbox.Batch.Project.ProjectManager                      (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                      as ProjectManager
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs                   (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Expr                                (Expr)
import           Flowbox.Luna.Data.AST.Module                              (Module)
import qualified Flowbox.Luna.Data.AST.Utils                               as AST
import           Flowbox.Luna.Data.AST.Zipper.Focus                        (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                        as Focus
import qualified Flowbox.Luna.Data.AST.Zipper.Zipper                       as Zipper
import           Flowbox.Luna.Data.Graph.Graph                             (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                             as Graph
import           Flowbox.Luna.Data.Graph.Node                              (Node)
import qualified Flowbox.Luna.Data.Graph.Node                              as Node
import           Flowbox.Luna.Data.GraphView.GraphView                     (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView                     as GraphView
import           Flowbox.Luna.Data.PropertyMap                             (PropertyMap)
import           Flowbox.Luna.Lib.LibManager                               (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                               as LibManager
import           Flowbox.Luna.Lib.Library                                  (Library)
import qualified Flowbox.Luna.Lib.Library                                  as Library
import qualified Flowbox.Luna.Passes.Analysis.ID.MaxID                     as MaxID
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias            as VarAlias
import qualified Flowbox.Luna.Passes.General.Luna.Luna                     as Luna
import qualified Flowbox.Luna.Passes.Transform.AST.IDFixer.IDFixer         as IDFixer
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder       as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser         as GraphParser
import qualified Flowbox.Luna.Passes.Transform.GraphView.Defaults.Defaults as Defaults
import           Flowbox.Prelude                                           hiding (focus, zipper)
import           Flowbox.System.Log.Logger



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Handler.Common"


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
    aa    <- Luna.runIO $ VarAlias.run ast
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
