---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Common (
    readonly,
    noresult,

    projectOp,
    libManagerOp,
    libraryOp,

    astOp,
    graphOp',
    nodeOp',
) where

import           Control.Monad.RWS
import           Flowbox.Batch.Batch                                 (Batch)
import qualified Flowbox.Batch.Batch                                 as Batch
import           Flowbox.Batch.Project.Project                       (Project)
import qualified Flowbox.Batch.Project.Project                       as Project
import           Flowbox.Batch.Project.ProjectManager                (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                as ProjectManager
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Crumb                   (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import qualified Flowbox.Luna.Data.AST.Zipper                        as Zipper
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                       as Graph
import           Flowbox.Luna.Data.Graph.Node                        (Node)
import qualified Flowbox.Luna.Data.Graph.Node                        as Node
import           Flowbox.Luna.Lib.LibManager                         (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                         as LibManager
import           Flowbox.Luna.Lib.Library                            (Library)
import qualified Flowbox.Luna.Lib.Library                            as Library
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias      as VarAlias
import qualified Flowbox.Luna.Passes.General.Luna.Luna               as Luna
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser   as GraphParser
import           Flowbox.Prelude                                     hiding (focus, zipper)



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


libManagerOp :: (Applicative m, Monad m)
             => Project.ID
             -> (Batch -> LibManager -> m (LibManager, r))
             -> Batch
             -> m (Batch, r)
libManagerOp projectID operation = projectOp projectID (\batch project -> do
    let libManager = Project.libs project
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


astOp :: (Applicative m, Monad m)
      => Library.ID
      -> Project.ID
      -> (Batch -> Module -> m (Module, r))
      -> Batch
      -> m (Batch, r)
astOp libID projectID operation = libraryOp libID projectID (\batch library -> do
    (newAst, r) <- operation batch $ Library.ast library
    let newLibrary = library { Library.ast = newAst }
    return (newLibrary, r))


graphOp' :: Breadcrumbs
         -> Library.ID
         -> Project.ID
         -> (Batch -> Graph -> IO (Graph, r))
         -> Batch
         -> IO (Batch, r)
graphOp' bc libID projectID operation = astOp libID projectID (\batch ast -> Luna.runIO $ do
    zipper <- Zipper.mk ast >>= Zipper.focusBreadcrumbs bc
    let focus = Zipper.getFocus zipper
    expr <- case focus of
        Zipper.FunctionFocus expr -> return expr
        _                         -> fail $ "Breadcrumbs are not focusing on function: " ++ (show bc)

    va    <- VarAlias.run ast
    graph <- GraphBuilder.run va expr
    (newGraph, r) <- liftIO $ operation batch graph
    ast' <- GraphParser.run newGraph expr

    newAst <- Zipper.modify (\_ -> Zipper.FunctionFocus ast') zipper >>= Zipper.close
    return (newAst, r))


nodeOp' :: Node.ID
        -> Breadcrumbs
        -> Library.ID
        -> Project.ID
        -> (Batch -> Node -> IO (Node, r))
        -> Batch
        -> IO (Batch, r)
nodeOp' nodeID bc libID projectID operation = graphOp' bc libID projectID (\batch graph -> do
    node <- Graph.lab graph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    (newNode, r) <- operation batch node
    let newGraph = Graph.updateNode (nodeID, newNode) graph
    return (newGraph, r))
