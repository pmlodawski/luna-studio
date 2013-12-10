---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handler.Common (
    readonly,
    readonly',
    noresult,

    projectOp',
    libManagerOp',
    libraryOp',

    projectOp,
    libManagerOp,
    libraryOp,

    astOp,
    graphOp',
) where

import           Control.Monad.RWS
import           Flowbox.Batch.Batch                                 (Batch (..))
import qualified Flowbox.Batch.Batch                                 as Batch
import           Flowbox.Batch.Project.Project                       (Project (..))
import qualified Flowbox.Batch.Project.Project                       as Project
import           Flowbox.Batch.Project.ProjectManager                (ProjectManager)
import qualified Flowbox.Batch.Project.ProjectManager                as ProjectManager
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Crumb                   (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import           Flowbox.Luna.Data.AST.Module                        (Module)
import qualified Flowbox.Luna.Data.AST.Zipper                        as Zipper
import           Flowbox.Luna.Data.Graph.Graph                       (Graph)
import           Flowbox.Luna.Lib.LibManager                         (LibManager)
import qualified Flowbox.Luna.Lib.LibManager                         as LibManager
import           Flowbox.Luna.Lib.Library                            (Library (..))
import qualified Flowbox.Luna.Lib.Library                            as Library
import qualified Flowbox.Luna.Passes.Analysis.VarAlias.VarAlias      as VarAlias
import qualified Flowbox.Luna.Passes.General.Luna.Luna               as Luna
import qualified Flowbox.Luna.Passes.Transform.Graph.Builder.Builder as GraphBuilder
import qualified Flowbox.Luna.Passes.Transform.Graph.Parser.Parser   as GraphParser
import           Flowbox.Prelude
import           Text.Show.Pretty



readonly :: Either String (a, r) -> Either String r
readonly op = case op of
    Left message -> Left message
    Right (_, r) -> Right r


readonly' :: IO (a, r) -> IO r
readonly' op = do
    (_, r) <- op
    return r


noresult :: Either String (a, r) -> Either String a
noresult op = case op of
    Left message -> Left message
    Right (a, _) -> Right a


projectManagerOp :: (Batch -> ProjectManager -> Either String (ProjectManager, r))
                 -> Batch
                 -> Either String (Batch, r)
projectManagerOp operation batch = do
    let aprojectManager = Batch.projectManager batch
    (newProjectManager, r) <- operation batch aprojectManager
    let newBatch = batch { Batch.projectManager = newProjectManager }
    return (newBatch, r)


projectManagerOp' :: (Batch -> ProjectManager -> IO (ProjectManager, r))
                  -> Batch
                  -> IO (Batch, r)
projectManagerOp' operation batch = do
    let aprojectManager = Batch.projectManager batch
    (newProjectManager, r) <- operation batch aprojectManager
    let newBatch = batch { Batch.projectManager = newProjectManager }
    return (newBatch, r)


projectOp :: Project.ID
          -> (Batch -> Project -> Either String (Project, r))
          -> Batch
          -> Either String (Batch, r)
projectOp projectID operation = projectManagerOp (\batch aprojectManager -> do
    project         <- ProjectManager.lab aprojectManager projectID <?> ("Wrong 'projectID' = " ++ show projectID)
    (newProject, r) <- operation batch project
    let newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
    return (newProjectManager, r))


projectOp' :: Project.ID
           -> (Batch -> Project -> IO (Project, r))
           -> Batch
           -> IO (Batch, r)
projectOp' projectID operation = projectManagerOp' (\batch aprojectManager -> runScript $ do
    project         <- ProjectManager.lab aprojectManager projectID <??> ("Wrong 'projectID' = " ++ show projectID)
    (newProject, r) <- scriptIO $ operation batch project
    let newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
    return (newProjectManager, r))


libManagerOp :: Project.ID
             -> (Batch -> LibManager -> Either String (LibManager, r))
             -> Batch
             -> Either String (Batch, r)
libManagerOp projectID operation = projectOp projectID (\batch project -> do
    let libManager = Project.libs project
    (newLibManager, r) <- operation batch libManager
    let newProject = project { Project.libs = newLibManager }
    return (newProject, r))


libManagerOp' :: Project.ID
              -> (Batch -> LibManager -> IO (LibManager, r))
              -> Batch
              -> IO (Batch, r)
libManagerOp' projectID operation = projectOp' projectID (\batch project -> do
    let libManager = Project.libs project
    (newLibManager, r) <- operation batch libManager
    let newProject = project { Project.libs = newLibManager }
    return (newProject, r))


libraryOp :: Library.ID
          -> Project.ID
          -> (Batch -> Library -> Either String (Library, r))
          -> Batch
          -> Either String (Batch, r)
libraryOp libID projectID operation = libManagerOp projectID (\batch libManager -> do
    library        <- LibManager.lab libManager libID <?> ("Wrong 'libID' = " ++ show libID)
    (newLibary, r) <- operation batch library
    let newLibManager = LibManager.updateNode (libID, newLibary) libManager
    return (newLibManager, r))


libraryOp' :: Library.ID
           -> Project.ID
           -> (Batch -> Library -> IO (Library, r))
           -> Batch
           -> IO (Batch, r)
libraryOp' libID projectID operation = libManagerOp' projectID (\batch libManager -> runScript $ do
    library        <- LibManager.lab libManager libID <??> ("Wrong 'libID' = " ++ show libID)
    (newLibary, r) <- scriptIO $ operation batch library
    let newLibManager = LibManager.updateNode (libID, newLibary) libManager
    return (newLibManager, r))


astOp :: Library.ID
      -> Project.ID
      -> (Batch -> Module -> Either String (Module, r))
      -> Batch
      -> Either String (Batch, r)
astOp libID projectID operation = libraryOp libID projectID (\batch library -> do
    (newAst, r) <- operation batch $ Library.ast library
    let newLibrary = library { Library.ast = newAst }
    return (newLibrary, r))


astOp' :: Library.ID
       -> Project.ID
       -> (Batch -> Module -> IO (Module, r))
       -> Batch
       -> IO (Batch, r)
astOp' libID projectID operation = libraryOp' libID projectID (\batch library -> do
    (newAst, r) <- operation batch $ Library.ast library
    let newLibrary = library { Library.ast = newAst }
    return (newLibrary, r))


graphOp' :: Breadcrumbs
        -> Library.ID
        -> Project.ID
        -> (Batch -> Graph -> IO (Graph, r))
        -> Batch
        -> IO (Batch, r)
graphOp' bc libID projectID operation = astOp' libID projectID (\batch ast -> Luna.runIO $ do
    let zipper = Zipper.mk ast
             >>= Zipper.focusBreadcrumbs bc
        focus  = fmap Zipper.getFocus zipper
        Just (Zipper.FunctionFocus expr) = focus

    --va    <- VarAlias.run ast
    --graph <- GraphBuilder.run va expr
    --(newGraph, r) <- liftIO $ operation batch graph
    --ast' <- GraphParser.run newGraph expr

    --let newAst = Zipper.modify (\_ -> Zipper.FunctionFocus ast')
    --           >>= Zipper.close
    ---- TODO [PM] defocus zipper
    --return (newAst, r))
    undefined)

--nodeOp :: Node.ID
--       -> Definition.ID
--       -> Library.ID
--       -> Project.ID
--       -> (Batch -> Node -> Either String (Node, r))
--       -> Batch
--       -> Either String (Batch, r)
--nodeOp nodeID defID libID projectID operation = graphOp defID libID projectID (\batch agraph -> do
--    node <- Graph.lab agraph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
--    (newNode, r) <- operation batch node
--    let newGraph = Graph.updateNode (nodeID, newNode) agraph
--    return (newGraph, r))
