---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Batch (
    Batch(..),
    empty,

    projects,
    createProject,
    openProject,
    closeProject,
    storeProject,
    setActiveProject,

    libraries,
    createLibrary,
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    buildLibrary,
    libraryRootDef,

    defsGraph ,
    addDefinition,
    updateDefinition ,
    removeDefinition,
    definitionChildren,
    definitionParent,

    nodesGraph ,
    addNode,
    updateNode,
    removeNode,
    connect,
    disconnect
) where

import           Flowbox.Batch.GraphView.EdgeView       (EdgeView(..))
import qualified Flowbox.Batch.GraphView.GraphView    as GraphView
import           Flowbox.Batch.GraphView.GraphView      (GraphView)
import qualified Flowbox.Batch.Project.Project        as Project
import           Flowbox.Batch.Project.Project          (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager as ProjectManager
import           Flowbox.Batch.Project.ProjectManager   (ProjectManager)
import qualified Flowbox.Luna.Builder.Builder         as Builder
import           Flowbox.Luna.Builder.Builder           (Builder(..))
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
import           Flowbox.Luna.Network.Graph.Node        (Node(..))
import qualified Flowbox.Luna.Tools.Serialize.Lib     as LibSerialization
import           Flowbox.System.UniPath                 (UniPath)


data Batch = Batch { projectManager  :: ProjectManager
                   , activeProjectID :: Project.ID
                   } deriving (Show)


empty :: Batch
empty = Batch ProjectManager.empty (-1)



activeProject :: Batch -> Maybe Project
activeProject (Batch pm apID) = do
    project <- ProjectManager.lab pm apID
    return project


activeProjectOp :: (Batch -> (Project.ID, Project) -> Either String (Project, r)) 
                         -> Batch
                         -> Either String (Batch, r)
activeProjectOp operation batch = 
    case activeProject batch of 
        Nothing                   -> Left "No active project set."
        Just project              -> let projectID = activeProjectID batch
                                     in case operation batch (projectID, project) of
            Left message          -> Left message
            Right (newProject, r) -> Right (newBatch, r) where
                                         aprojectManager   = projectManager batch
                                         newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
                                         newBatch          = batch { projectManager = newProjectManager }

activeProjectOp' :: (Batch -> (Project.ID, Project) -> IO (Either String (Project, r)))
                 -> Batch
                 -> IO (Either String (Batch, r))
activeProjectOp' operation batch = 
    case activeProject batch of 
        Nothing                   -> return $ Left "No active project set."
        Just project              -> do 
            let projectID = activeProjectID batch
            opr <- operation batch (projectID, project)
            case opr of
                Left message          -> return $ Left message
                Right (newProject, r) -> do let aprojectManager   = projectManager batch
                                                newProjectManager = ProjectManager.updateNode (projectID, newProject) aprojectManager
                                                newBatch          = batch { projectManager = newProjectManager }
                                            return $ Right (newBatch, r)


libManagerOp :: (Batch -> LibManager -> Either String (LibManager, r))
             -> Batch 
             -> Either String (Batch, r)
libManagerOp operation = 
    activeProjectOp (\batch (_, project) -> let
        libManager = Project.libs project
        in case operation batch libManager of 
            Left message             -> Left message
            Right (newLibManager, r) -> Right (newProject, r) where
                                            newProject = project { Project.libs = newLibManager })


libManagerOp' :: (Batch -> LibManager -> IO (Either String (LibManager, r)))
              -> Batch 
              -> IO (Either String (Batch, r))
libManagerOp' operation = 
    activeProjectOp' (\batch (_, project) -> do 
        let libManager = Project.libs project
        opr <- operation batch libManager
        case opr of 
            Left message             -> return $ Left message
            Right (newLibManager, r) -> do let newProject = project { Project.libs = newLibManager }
                                           return $ Right (newProject, r))


libraryOp :: Library.ID
          -> (Batch -> Library -> Either String (Library, r))
          -> Batch
          -> Either String (Batch, r)
libraryOp libID operation =
    libManagerOp (\batch libManager  -> case LibManager.lab libManager libID of
            Nothing                  -> Left $ "Wrong `libID` = " ++ show libID
            Just library             -> case operation batch library of 
                Left message         -> Left message
                Right (newLibary, r) -> Right (newLibManager, r) where
                    newLibManager = LibManager.updateNode (libID, newLibary) libManager)


libraryOp' :: Library.ID
           -> (Batch -> Library -> IO (Either String (Library, r)))
           -> Batch
           -> IO (Either String (Batch, r))
libraryOp' libID operation =
    libManagerOp' (\batch libManager  -> case LibManager.lab libManager libID of
        Nothing                  -> return $ Left $ "Wrong `libID` = " ++ show libID
        Just library             -> do
            opr <- operation batch library 
            case opr of 
                Left message         -> return $ Left message
                Right (newLibary, r) -> do 
                    let newLibManager = LibManager.updateNode (libID, newLibary) libManager
                    return $ Right (newLibManager, r))


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


readonly :: Either String (a, r) -> Either String r
readonly op = case op of 
    Left message -> Left message
    Right (_, r) -> Right r


readonly' :: IO (Either String (a, r)) -> IO (Either String r)
readonly' op = do
    o <- op
    case o of 
        Left message -> return $ Left message
        Right (_, r) -> return $ Right r


noresult :: Either String (a, r) -> Either String a
noresult op = case op of 
    Left message -> Left message
    Right (a, _) -> Right a


-------- Projects -------------------------------------------------------------

projects :: Batch -> [(Project.ID, Project)]
projects batch = ProjectManager.projects (projectManager batch)



createProject :: Project -> Batch -> IO ()
createProject project _ = ProjectManager.createProject project


openProject :: Project -> Batch -> IO (Batch, (Project.ID, Project))
openProject project batch = do
    let aprojectManager = projectManager batch
    (newProjectManager, newP) <- ProjectManager.openProject aprojectManager project
    let newBatch = batch {projectManager = newProjectManager}
    return (newBatch, newP)



closeProject :: Project.ID -> Batch -> IO Batch
closeProject projectID batch = do
    let aprojectManager = projectManager batch
    newProjectManager <- ProjectManager.closeProject aprojectManager projectID
    let newBatch = batch {projectManager = newProjectManager}
    return newBatch


storeProject :: Project.ID -> Batch -> IO ()
storeProject projectID batch = do
    let aprojectManager = projectManager batch
    _ <- ProjectManager.storeProject aprojectManager projectID
    return ()


setActiveProject :: Project.ID -> Batch -> Batch
setActiveProject projectID batch = newBatch where
    newBatch = batch { activeProjectID = projectID }


-------- Libraries ------------------------------------------------------------

libraries :: Batch -> Either String [(Library.ID, Library)]
libraries = readonly . libManagerOp (\_ libManager -> 
    let r = LibManager.labNodes libManager 
    in Right (libManager, r))


createLibrary :: String -> UniPath -> Batch -> Either String (Batch, (Library.ID, Library))
createLibrary libName libPath = libManagerOp (\_ libManager -> 
    let library                = Library.make libName libPath
        (newLibManager, libID) = LibManager.insNewNode library libManager
    in Right (newLibManager, (libID, library)))


loadLibrary :: Library -> Batch -> IO (Either String (Batch, (Library.ID, Library)))
loadLibrary library = libManagerOp' (\_ libManager -> do
    let lname = Library.name library
        lpath = Library.path library
    r <- LibManager.loadLibrary lname lpath libManager
    return $ Right r)


unloadLibrary :: Library.ID -> Batch -> Either String Batch
unloadLibrary libID = noresult . libManagerOp (\_ libManager -> 
    let newLibManager = LibManager.unloadLibrary libID libManager
    in Right (newLibManager, ()))


storeLibrary :: Library.ID -> Batch -> IO (Either String ())
storeLibrary libID = readonly' . libraryOp' libID (\_ library -> do
    LibSerialization.storeLibrary library
    return $ Right (library, ()))


buildLibrary :: Library.ID -> Batch -> IO (Either String ())
buildLibrary libID = readonly' . libraryOp' libID (\batch library -> do
    let Just proj = activeProject batch
        path = Project.path proj
        b = Builder path
    Builder.buildLibrary b library
    return $ Right (library, ()))


libraryRootDef :: Library.ID -> Batch -> Either String (Definition.ID, Definition)
libraryRootDef libID = readonly . definitionOp Library.rootDefID libID (\_ definition ->  
    Right (definition, (Library.rootDefID, definition)))


-------- Definitions ----------------------------------------------------------

defsGraph :: Library.ID -> Batch -> Either String DefManager
defsGraph libID = readonly . defManagerOp libID (\_ defManager -> 
    Right (defManager, defManager))


--newDefinition :: IORef Project -> Maybe TTypes.Type -> Maybe (Vector Import)
--                            -> Maybe Attrs_Types.Flags -> Maybe Attrs_Types.Attributes
--                            -> IO Definition
--newDefinition _ ttype timports tflags tattrs = do 
--    putStrLn "Creating new definition...\t\tsuccess!"
--    return $ Definition ttype timports tflags tattrs (Just 0) (Just 0)


addDefinition :: Definition -> Definition.ID -> Library.ID -> Batch 
              -> Either String (Batch, Definition.ID)
addDefinition definition parentID libID = defManagerOp libID (\_ defManager ->
    case DefManager.gelem parentID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right $ DefManager.addNewToParent (parentID, definition) defManager)


updateDefinition :: (Definition.ID, Definition) -> Library.ID -> Batch -> Either String Batch
updateDefinition (defID, def) libID = noresult . definitionOp defID libID (\_ _ ->
    Right (def, def))


removeDefinition :: Definition.ID -> Library.ID -> Batch -> Either String Batch 
removeDefinition defID libID = noresult . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right (newDefManager, ()) where 
                        newDefManager = DefManager.delete defID defManager)


definitionChildren :: Definition.ID -> Library.ID -> Batch
                   -> Either String [(Definition.ID, Definition)]
definitionChildren defID libID = readonly . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False -> Left "Wrong `defID`"
        True  -> Right (defManager, children) where
                       children = DefManager.children defManager defID)


definitionParent :: Definition.ID -> Library.ID -> Batch -> Either String (Definition.ID, Definition)
definitionParent defID libID = readonly . defManagerOp libID (\_ defManager -> 
    case DefManager.gelem defID defManager of 
        False           -> Left "Wrong `defID`"
        True            -> case DefManager.parent defManager defID of
            Nothing     -> Left "Definition has no parent"
            Just parent -> Right (defManager, parent))


-------- Graphs ---------------------------------------------------------------

nodesGraph :: Definition.ID -> Library.ID -> Batch -> Either String GraphView
nodesGraph defID libID = readonly . graphOp defID libID (\_ agraph -> 
    Right (agraph, GraphView.fromGraph agraph))


addNode :: Node -> Definition.ID -> Library.ID -> Batch -> Either String (Batch, Node.ID)
addNode node defID libID = graphOp defID libID (\_ agraph -> 
    Right $ Graph.insNewNode node agraph)


updateNode :: (Node.ID, Node) -> Definition.ID -> Library.ID -> Batch -> Either String Batch
updateNode (nodeID, node) defID libID = noresult . graphOp defID libID (\_ agraph -> 
    case Graph.gelem nodeID agraph of 
        False -> Left "Wrong `nodeID`"
        True  -> Right (newGraph, ()) where
             newGraph      = Graph.updateNode (nodeID, node) agraph)


removeNode :: Node.ID -> Definition.ID -> Library.ID ->  Batch -> Either String Batch
removeNode nodeID defID libID = noresult . graphOp defID libID (\_ agraph -> 
    case Graph.gelem nodeID agraph of 
        False -> Left "Wrong `nodeID`"
        True  -> Right (newGraph, ()) where
            newGraph      = Graph.delNode nodeID agraph)


connect :: Node.ID -> [Int] -> Node.ID -> Int -> Definition.ID -> Library.ID -> Batch -> Either String Batch
connect srcNodeID srcPort dstNodeID dstPort defID libID = noresult . graphOp defID libID (\_ agraph -> 
    case Graph.gelem srcNodeID agraph of 
        False     -> Left "Wrong `srcNodeID`"
        True      -> case Graph.gelem dstNodeID agraph of 
            False -> Left "Wrong `dstNodeID`"
            True  -> 
                let newGraph = GraphView.toGraph 
                             $ GraphView.insEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) 
                             $ GraphView.fromGraph agraph
                in Right (newGraph, ()))

disconnect :: Node.ID -> [Int] -> Node.ID -> Int -> Definition.ID -> Library.ID -> Batch -> Either String Batch
disconnect srcNodeID srcPort dstNodeID dstPort defID libID = noresult . graphOp defID libID (\_ agraph -> 
    case Graph.gelem srcNodeID agraph of 
        False     -> Left "Wrong `srcNodeID`"
        True      -> case Graph.gelem dstNodeID agraph of 
            False -> Left "Wrong `dstNodeID`"
            True  -> 
                let newGraph = GraphView.toGraph 
                             $ GraphView.delLEdge (srcNodeID, dstNodeID, EdgeView srcPort dstPort) 
                             $ GraphView.fromGraph agraph
                in Right (newGraph, ()))
