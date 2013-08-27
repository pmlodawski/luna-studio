---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Handlers.Common (
    readonly,
    readonly',
    noresult,

    activeProject,
    activeProjectOp,
    activeProjectOp',
    libManagerOp ,
    libManagerOp',
    libraryOp,
    libraryOp',
    defManagerOp,
    definitionOp,
    graphOp,
) where

import           Flowbox.Batch.Batch                    (Batch(..))
import qualified Flowbox.Batch.Project.Project        as Project
import           Flowbox.Batch.Project.Project          (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import qualified Flowbox.Luna.Lib.LibManager          as LibManager
import           Flowbox.Luna.Lib.LibManager            (LibManager)
import qualified Flowbox.Luna.Lib.Library             as Library
import           Flowbox.Luna.Lib.Library               (Library(..))
import qualified Flowbox.Luna.Network.Def.DefManager  as DefManager
import           Flowbox.Luna.Network.Def.DefManager    (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition  as Definition
import           Flowbox.Luna.Network.Def.Definition    (Definition(..))
import           Flowbox.Luna.Network.Graph.Graph       (Graph)



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


activeProject :: Batch -> Maybe Project
activeProject (Batch pm apID) = do
    project <- ProjectManager.lab pm apID
    return project


activeProjectOp :: (Batch -> (Project.ID, Project) -> Either String (Project, r)) 
                         -> Batch
                         -> Either String (Batch, r)
activeProjectOp operation batch = case activeProject batch of 
    Nothing                   -> Left "No active project set."
    Just project              -> let projectID = activeProjectID batch
                                 in case operation batch (projectID, project) of
        Left message          -> Left message
        Right (newProject, r) -> Right (newBatch, r) where
                                     aprojectManager   = projectManager batch
                                     newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
                                     newBatch          = batch { projectManager = newProjectManager }


activeProjectOp' :: (Batch -> (Project.ID, Project) -> IO (Project, r))
                 -> Batch
                 -> IO (Batch, r)
activeProjectOp' operation batch = case activeProject batch of 
    Nothing                   -> error "No active project set."
    Just project              -> do 
        let projectID = activeProjectID batch
        (newProject, r) <- operation batch (projectID, project)

        let aprojectManager   = projectManager batch
            newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
            newBatch          = batch { projectManager = newProjectManager }
        return (newBatch, r)

libManagerOp :: (Batch -> LibManager -> Either String (LibManager, r))
             -> Batch 
             -> Either String (Batch, r)
libManagerOp operation = activeProjectOp (\batch (_, project) -> let
    libManager = Project.libs project
    in case operation batch libManager of 
        Left message             -> Left message
        Right (newLibManager, r) -> Right (newProject, r) where
                                            newProject = project { Project.libs = newLibManager })


libManagerOp' :: (Batch -> LibManager -> IO (LibManager, r))
              -> Batch 
              -> IO (Batch, r)
libManagerOp' operation = activeProjectOp' (\batch (_, project) -> do 
    let libManager = Project.libs project
    (newLibManager, r) <- operation batch libManager
    let newProject = project { Project.libs = newLibManager }
    return (newProject, r))


libraryOp :: Library.ID
          -> (Batch -> Library -> Either String (Library, r))
          -> Batch
          -> Either String (Batch, r)
libraryOp libID operation =
    libManagerOp (\batch libManager  -> case LibManager.lab libManager libID of
        Nothing                      -> Left $ "Wrong `libID` = " ++ show libID
        Just library                 -> case operation batch library of 
            Left message             -> Left message
            Right (newLibary, r)     -> Right (newLibManager, r) where
                newLibManager = LibManager.updateNode (libID, newLibary) libManager)


libraryOp' :: Library.ID
           -> (Batch -> Library -> IO (Library, r))
           -> Batch
           -> IO (Batch, r)
libraryOp' libID operation =
    libManagerOp' (\batch libManager  -> case LibManager.lab libManager libID of
        Nothing                  -> error $ "Wrong `libID` = " ++ show libID
        Just library             -> do
            (newLibary, r) <- operation batch library 
            let newLibManager = LibManager.updateNode (libID, newLibary) libManager
            return (newLibManager, r))


defManagerOp :: Library.ID
             -> (Batch -> DefManager -> Either String (DefManager, r))
             -> Batch 
             -> Either String (Batch, r)
defManagerOp libID operation = 
    libraryOp libID (\batch library -> let
        defManager = Library.defs library
        in case operation batch defManager of 
            Left message             -> Left message
            Right (newDefManager, r) -> Right (newLibrary, r) where
                newLibrary = library { Library.defs = newDefManager })


definitionOp :: Definition.ID
             -> Library.ID 
             -> (Batch -> Definition -> Either String (Definition, r))
             -> Batch 
             -> Either String (Batch, r)
definitionOp defID libID operation = 
    defManagerOp libID (\batch defManager -> 
    case DefManager.lab defManager defID of 
        Nothing                      -> Left $ "Wrong `defID` = " ++ show defID
        Just definition              -> case operation batch definition of 
            Left message             -> Left message
            Right (newDefinition, r) -> Right (newDefManager, r) where
                newDefManager = DefManager.updateNode (defID, newDefinition) defManager)



graphOp :: Definition.ID
        -> Library.ID 
        -> (Batch -> Graph -> Either String (Graph, r))
        -> Batch 
        -> Either String (Batch, r)
graphOp defID libID operation = 
    definitionOp defID libID (\batch definition -> let 
        agraph = Definition.graph definition
        in case operation batch agraph of 
            Left message        -> Left message
            Right (newGraph, r) -> Right (newDefinition, r) where
                newDefinition = definition {Definition.graph =  newGraph})

