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

    projectOp',
    libManagerOp',
    libraryOp',
    
    projectOp,
    libManagerOp,
    libraryOp,
    defManagerOp,
    definitionOp,
    graphOp,
    graphViewOp,
    nodeOp,
) where

import qualified Flowbox.Batch.Batch                  as Batch
import           Flowbox.Batch.Batch                    (Batch(..))
import qualified Flowbox.Batch.GraphView.GraphView    as GraphView
import           Flowbox.Batch.GraphView.GraphView      (GraphView)
import qualified Flowbox.Batch.Project.Project        as Project
import           Flowbox.Batch.Project.Project          (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)
import           Flowbox.Control.Error                  
import qualified Flowbox.Luna.Lib.LibManager          as LibManager
import           Flowbox.Luna.Lib.LibManager            (LibManager)
import qualified Flowbox.Luna.Lib.Library             as Library
import           Flowbox.Luna.Lib.Library               (Library(..))
import qualified Flowbox.Luna.Network.Def.DefManager  as DefManager
import           Flowbox.Luna.Network.Def.DefManager    (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition  as Definition
import           Flowbox.Luna.Network.Def.Definition    (Definition(..))
import qualified Flowbox.Luna.Network.Graph.Graph     as Graph
import           Flowbox.Luna.Network.Graph.Graph       (Graph)
import qualified Flowbox.Luna.Network.Graph.Node      as Node
import           Flowbox.Luna.Network.Graph.Node        (Node)
import Debug.Trace



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
projectOp' projectID operation = projectManagerOp' (\batch aprojectManager -> eRunScript $ do
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
libraryOp' libID projectID operation = libManagerOp' projectID (\batch libManager -> eRunScript $ do
    library        <- LibManager.lab libManager libID <??> ("Wrong 'libID' = " ++ show libID)
    (newLibary, r) <- scriptIO $ operation batch library 
    let newLibManager = LibManager.updateNode (libID, newLibary) libManager
    return (newLibManager, r))


defManagerOp :: Library.ID
             -> Project.ID
             -> (Batch -> DefManager -> Either String (DefManager, r))
             -> Batch 
             -> Either String (Batch, r)
defManagerOp libID projectID operation = libraryOp libID projectID (\batch library -> do
    let defManager = Library.defs library
    (newDefManager, r) <- operation batch defManager
    let newLibrary = library { Library.defs = newDefManager }
    return (newLibrary, r))


definitionOp :: Definition.ID
             -> Library.ID 
             -> Project.ID
             -> (Batch -> Definition -> Either String (Definition, r))
             -> Batch 
             -> Either String (Batch, r)
definitionOp defID libID projectID operation = defManagerOp libID projectID (\batch defManager -> do
    definition <- DefManager.lab defManager defID <?> ("Wrong 'defID' = " ++ show defID)
    (newDefinition, r) <- operation batch definition
    let newDefManager = DefManager.updateNode (defID, newDefinition) defManager
    return (newDefManager, r))


graphOp :: Definition.ID
        -> Library.ID 
        -> Project.ID
        -> (Batch -> Graph -> Either String (Graph, r))
        -> Batch 
        -> Either String (Batch, r)
graphOp defID libID projectID operation = definitionOp defID libID projectID (\batch definition -> do
    let agraph = Definition.graph definition
    (newGraph, r) <- operation batch agraph
    let newDefinition = definition {Definition.graph =  newGraph}
    return (newDefinition, r))


graphViewOp :: Definition.ID
        -> Library.ID 
        -> Project.ID
        -> (Batch -> GraphView -> Either String (GraphView, r))
        -> Batch 
        -> Either String (Batch, r)
graphViewOp defID libID projectID operation = definitionOp defID libID projectID (\batch definition -> do
    let agraph = Definition.graph definition
    graphView <- GraphView.fromGraph agraph
    (newGraphView, r) <- operation batch graphView
    newGraph <- GraphView.toGraph newGraphView
    t <- GraphView.fromGraph newGraph
    let newDefinition = definition {Definition.graph =  newGraph}
    return (traceShow newGraphView $ traceShow newGraph $ traceShow t newDefinition, r))


nodeOp :: Node.ID
       -> Definition.ID
       -> Library.ID 
       -> Project.ID
       -> (Batch -> Node -> Either String (Node, r))
       -> Batch 
       -> Either String (Batch, r)
nodeOp nodeID defID libID projectID operation = graphOp defID libID projectID (\batch agraph -> do 
    node <- Graph.lab agraph nodeID <?> ("Wrong 'nodeID' = " ++ show nodeID)
    (newNode, r) <- operation batch node
    let newGraph = Graph.updateNode (nodeID, newNode) agraph
    return (newGraph, r))