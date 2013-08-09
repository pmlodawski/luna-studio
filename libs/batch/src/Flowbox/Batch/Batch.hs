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
    loadLibrary,
    unloadLibrary,
    storeLibrary,
    libraryRootDef

) where


import qualified Flowbox.Batch.Project.Project            as Project
import           Flowbox.Batch.Project.Project              (Project(..))
import qualified Flowbox.Batch.Project.ProjectManager     as ProjectManager
import           Flowbox.Batch.Project.ProjectManager       (ProjectManager)
import qualified Flowbox.Luna.Core                        as Core
import           Flowbox.Luna.Core                          (Core(..))
import qualified Flowbox.Luna.Lib.LibManager              as LibManager
import           Flowbox.Luna.Lib.LibManager                (LibManager)
import qualified Flowbox.Luna.Lib.Library                 as Library
import           Flowbox.Luna.Lib.Library                   (Library(..))
import qualified Flowbox.Luna.Network.Attributes          as Attributes
import           Flowbox.Luna.Network.Attributes            (Attributes)
import qualified Flowbox.Luna.Network.Def.DefManager      as DefManager
import           Flowbox.Luna.Network.Def.DefManager        (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition      as Definition
import           Flowbox.Luna.Network.Def.Definition        (Definition(..))
import qualified Flowbox.Luna.Network.Flags               as Flags
import qualified Flowbox.Luna.Network.Graph.Graph         as Graph
import qualified Flowbox.Luna.System.UniPath              as UniPath
import           Flowbox.Luna.System.UniPath                (UniPath)
import qualified Flowbox.Luna.Type.Type                   as Type


data Batch = Batch { projectManager  :: ProjectManager
                   , activeProjectID :: Project.ID
                   }

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



activeCoreOp :: (Batch -> Core -> Either String (Core, r))
             -> Batch
             -> Either String (Batch, r)
activeCoreOp operation = 
    activeProjectOp (\batch (_, project) -> let
        core = Project.core project
        in case operation batch core of 
            Left message       -> Left message
            Right (newCore, r) -> Right (newProject, r) where
                                        newProject = project {Project.core = newCore})


activeCoreOp' :: (Batch -> Core -> IO (Either String (Core, r)))
              -> Batch
              -> IO (Either String (Batch, r))
activeCoreOp' operation = 
    activeProjectOp' (\batch (_, project) -> do
        let core = Project.core project
        opr <- operation batch core
        case opr of 
            Left message       -> return $ Left message
            Right (newCore, r) -> do let newProject = project {Project.core = newCore}
                                     return $ Right (newProject, r))


activeLibManagerOp :: (Batch -> LibManager -> Either String (LibManager, r))
                   -> Batch 
                   -> Either String (Batch, r)
activeLibManagerOp operation = 
    activeCoreOp (\batch core -> let
        libManager = Core.libManager core
        in case operation batch libManager of 
            Left message             -> Left message
            Right (newLibManager, r) -> Right (newCore, r) where
                                            newCore = core { Core.libManager = newLibManager })


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
projects batch = projectList where
    aprojectManager = projectManager batch
    projectList     = ProjectManager.projects aprojectManager


createProject :: Batch -> Project -> IO ()
createProject _ = ProjectManager.createProject


openProject :: Batch -> Project -> IO (Batch, (Project.ID, Project))
openProject batch project = do
    let aprojectManager = projectManager batch
    (newProjectManager, newP) <- ProjectManager.openProject aprojectManager project
    let newBatch = batch {projectManager = newProjectManager}
    return (newBatch, newP)



closeProject :: Batch -> Project.ID -> IO Batch
closeProject batch projectID = do
    let aprojectManager = projectManager batch
    newProjectManager <- ProjectManager.closeProject aprojectManager projectID
    let newBatch = batch {projectManager = newProjectManager}
    return newBatch



storeProject :: Batch -> Project.ID -> IO ()
storeProject batch projectID = do
    let aprojectManager = projectManager batch
    _ <- ProjectManager.storeProject aprojectManager projectID
    return ()


setActiveProject :: Batch -> Project.ID -> Batch
setActiveProject batch projectID = newBatch where
    newBatch = batch { activeProjectID = projectID }


-------- Libraries ------------------------------------------------------------

libraries :: Batch -> Either String [(Library.ID, Library)]
libraries = readonly . activeLibManagerOp (\batch libManager -> let 
    r = LibManager.labNodes libManager 
    in Right (libManager, r))

--createLibrary :: Batch -> Library -> Library ???
--createLibrary _ library = library


loadLibrary :: Library -> Batch -> IO (Either String (Batch, (Library.ID, Library)))
loadLibrary library = activeCoreOp' (\batch core -> do
    let (newCore, newLibrary, newLibID) = Core.loadLibrary core library
    return $ Right (newCore, (newLibID, newLibrary)))


unloadLibrary :: Library.ID -> Batch -> Either String Batch
unloadLibrary libraryID = noresult . activeCoreOp (\batch core ->let 
    newCore = Core.unloadLibrary core libraryID
    in Right (newCore, ()))


storeLibrary :: Library.ID -> Batch -> IO (Either String ())
storeLibrary libraryID = readonly' . activeCoreOp' (\batch core -> do
    putStrLn "call storeLibrary - NOT YET IMPLEMENTED"
    return $ Right (core, ()))


libraryRootDef :: Library.ID -> Batch -> Either String (Definition.ID, Definition)
libraryRootDef libraryID = readonly . activeCoreOp (\batch core -> let 
    libManager = Core.libManager core
    in case LibManager.lab libManager libraryID of 
        Nothing      -> Left "Wrong `libraryID"
        Just library -> let 
            rootDefID' = Library.rootDefID library 
            in case Core.nodeDefByID core rootDefID' of
                Nothing      -> Left "Wrong `rootDefID` in `library`"
                Just rootDef -> Right (core, (rootDefID', rootDef)))


-------- Definitions ----------------------------------------------------------


-------- Graphs ---------------------------------------------------------------