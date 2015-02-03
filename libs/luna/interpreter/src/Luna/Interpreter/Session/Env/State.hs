---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.State where

import qualified Control.Concurrent.MVar    as MVar
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.IntSet                (IntSet)
import qualified Data.IntSet                as IntSet
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Data.Monoid                ((<>))
import           Data.MultiSet              (MultiSet)
import qualified Data.MultiSet              as MultiSet
import           Data.Set                   (Set)
import qualified Data.Set                   as Set

import           Control.Monad.Catch                         (bracket_)
import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Control.Error
import           Flowbox.Data.MapForest                      (MapForest)
import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Data.Mode                           (Mode)
import           Flowbox.Data.SetForest                      (SetForest)
import qualified Flowbox.Data.SetForest                      as SetForest
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (Location, loc)
import           Luna.DEP.AST.Module                             (Module)
import qualified Luna.Syntax.Graph.Flags                            as Flags
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Data.DefPoint      as DefPoint
import qualified Luna.Interpreter.Session.Env.Env            as Env
import           Luna.Interpreter.Session.Env.Session        (Session)
import           Luna.Interpreter.Session.Error              (Error)
import qualified Luna.Interpreter.Session.Error              as Error
import qualified Luna.Interpreter.Session.Memory.Config      as Memory
import           Luna.Interpreter.Session.ProfileInfo        (ProfileInfo)
import qualified Luna.Interpreter.Session.ProfileInfo        as ProfileInfo
import           Luna.Interpreter.Session.TargetHS.Reload    (Reload, ReloadMap)
import           Luna.Lib.Lib                                (Library)
import qualified Luna.Lib.Lib                                as Library
import           Luna.Lib.Manager                            (LibManager)
import qualified Luna.Lib.Manager                            as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias              as Alias
import qualified Luna.Pass.Transform.Graph.Builder.Builder   as GraphBuilder
import qualified Luna.Syntax.AST                             as AST
import           Luna.Syntax.Control.Focus                   (Focus)
import qualified Luna.Syntax.Control.Focus                   as Focus
import qualified Luna.Syntax.Control.Zipper                  as Zipper
import           Luna.Syntax.Expr                            (Expr)
import qualified Luna.Syntax.Expr                            as Expr
import           Luna.Syntax.Graph.Flags                     (Flags)
import           Luna.Syntax.Graph.Graph                     (Graph)
import qualified Luna.Syntax.Graph.Node                      as Node
import           Luna.Syntax.Graph.PropertyMap               (PropertyMap)
import qualified Luna.Syntax.Graph.PropertyMap               as PropertyMap
import           Luna.Syntax.Graph.View.Default.DefaultsMap  (DefaultsMap)



---- Env.cached -----------------------------------------------------------

getCached :: Session mm (MapForest CallPoint CacheInfo)
getCached = gets $ view Env.cached


cachedInsert :: CallPointPath -> CacheInfo -> Session mm ()
cachedInsert = (modify . over Env.cached) .: MapForest.insert


cachedDelete :: CallPointPath -> Session mm ()
cachedDelete = modify . over Env.cached . MapForest.delete


cachedLookup :: CallPointPath -> Session mm (Maybe CacheInfo)
cachedLookup callPointPath = MapForest.lookup callPointPath <$> getCached


cachedClear :: Session mm ()
cachedClear = modify (Env.cached .~ def)

---- Env.watchPoints ------------------------------------------------------

addWatchPoint :: CallPointPath -> Session mm ()
addWatchPoint callPath = modify (Env.watchPoints %~ SetForest.insert callPath)


deleteWatchPoint :: CallPointPath -> Session mm ()
deleteWatchPoint callPath = modify (Env.watchPoints %~ SetForest.delete callPath)


cleanWatchPoints :: Session mm ()
cleanWatchPoints = modify (Env.watchPoints .~ def)


getWatchPoints :: Session mm (SetForest CallPoint)
getWatchPoints = gets (view Env.watchPoints)

---- Env.reloadMap --------------------------------------------------------

addReload :: Library.ID -> Reload -> Session mm ()
addReload libraryID reload = modify (Env.reloadMap %~ update) where
    update = Map.alter (Just . (<> reload) . Maybe.fromMaybe def) libraryID


getReloads :: Session mm ReloadMap
getReloads = gets $ view Env.reloadMap


cleanReloads :: Session mm ()
cleanReloads = modify (Env.reloadMap .~ mempty)

---- Env.allReady ---------------------------------------------------------

setAllReady :: Bool -> Session mm ()
setAllReady = modify . set Env.allReady


getAllReady :: Session mm Bool
getAllReady = gets $ view Env.allReady

---- Env.fragileOperation -------------------------------------------------

getFragile :: Session mm Env.FragileMVar
getFragile = gets $ view Env.fragileOperation


fragile :: Session mm a -> Session mm a
fragile action = do
    f <- getFragile
    lift (bracket_ (liftIO $ MVar.takeMVar f) (liftIO $ MVar.putMVar f ()) $ runEitherT action) >>= hoistEither

---- Env.dependentNodes ---------------------------------------------------

getDependentNodes :: Session mm (Map CallPoint IntSet)
getDependentNodes = gets $ view Env.dependentNodes


getDependentNodesOf :: CallPoint -> Session mm IntSet
getDependentNodesOf callPoint =
    Maybe.fromMaybe def . Map.lookup callPoint <$> getDependentNodes


insertDependentNode :: CallPoint -> Node.ID -> Session mm ()
insertDependentNode callPoint nodeID =
    modify (Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.insert nodeID . Maybe.fromMaybe def


insertDependentNodes :: CallPoint -> IntSet -> Session mm ()
insertDependentNodes callPoint nodeIDs =
    modify (Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.union nodeIDs . Maybe.fromMaybe def


deleteDependentNodes :: CallPoint -> Session mm ()
deleteDependentNodes = modify . over Env.dependentNodes . Map.delete


deleteDependentNode :: CallPoint -> Node.ID -> Session mm ()
deleteDependentNode callPoint nodeID =
    modify (Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.delete nodeID . Maybe.fromMaybe def


cleanDependentNodes :: Session mm ()
cleanDependentNodes = modify (Env.dependentNodes .~ def)

---- Env.profileInfos -----------------------------------------------------

cleanProfileInfos :: Session mm ()
cleanProfileInfos = modify $ Env.profileInfos .~ def


getProfileInfos :: Session mm (MapForest CallPoint ProfileInfo)
getProfileInfos = gets $ view Env.profileInfos


insertProfileInfo :: CallPointPath -> ProfileInfo -> Session mm ()
insertProfileInfo callPointPath info =
    modify (Env.profileInfos %~ MapForest.insert callPointPath info)


profile :: CallPointPath -> Session mm a -> Session mm a
profile callPointPath action = do
    (r, info) <- ProfileInfo.profile action
    whenVisible callPointPath $ insertProfileInfo callPointPath info
    return r

---- Env.timeVar ----------------------------------------------------------

getTimeVar :: Session mm Double
getTimeVar = gets $ view Env.timeVar


setTimeVar :: Double -> Session mm ()
setTimeVar = modify . set Env.timeVar

---- Env.timeRefs ---------------------------------------------------------

insertTimeRef :: CallPoint -> Session mm ()
insertTimeRef callPoint = modify (Env.timeRefs %~ Set.insert callPoint)


deleteTimeRef :: CallPoint -> Session mm ()
deleteTimeRef callPoint = modify (Env.timeRefs %~ Set.delete callPoint)


getTimeRefs :: Session mm (Set CallPoint)
getTimeRefs = gets $ view Env.timeRefs


cleanTimeRefs :: Session mm ()
cleanTimeRefs = modify $ Env.timeRefs .~ def

---- Env.serializationModes -----------------------------------------------

getSerializationModesMap :: Session mm (MapForest CallPoint (MultiSet Mode))
getSerializationModesMap = gets $ view Env.serializationModes


lookupSerializationModes :: CallPointPath -> Session mm (Maybe (MultiSet Mode))
lookupSerializationModes callPointPath =
    MapForest.lookup callPointPath <$> getSerializationModesMap


getSerializationModes :: CallPointPath -> Session mm (MultiSet Mode)
getSerializationModes callPointPath =
    Maybe.fromMaybe MultiSet.empty <$> lookupSerializationModes callPointPath


insertSerializationModes :: CallPointPath -> MultiSet Mode -> Session mm ()
insertSerializationModes callPointPath modes =
    modify (Env.serializationModes %~ MapForest.alter ins callPointPath) where
        ins  Nothing = Just modes
        ins (Just s) = Just $ MultiSet.union s modes


deleteSerializationModes :: CallPointPath -> MultiSet Mode -> Session mm ()
deleteSerializationModes callPointPath modes =
    modify (Env.serializationModes %~ MapForest.alter del callPointPath) where
        del  Nothing = Just modes
        del (Just s) = Just $ MultiSet.difference s modes


deleteAllSerializationModes :: CallPointPath -> Session mm ()
deleteAllSerializationModes = modify . over Env.serializationModes . MapForest.delete


cleanSerializationModes :: Session mm ()
cleanSerializationModes = modify (Env.serializationModes .~ def)

---- Env.memoryConfig -----------------------------------------------------

getMemoryConfig :: Session mm Memory.Config
getMemoryConfig = gets $ view Env.memoryConfig


setMemoryConfig :: Memory.Config -> Session mm ()
setMemoryConfig = modify . set Env.memoryConfig

---- Env.memoryManager ----------------------------------------------------

getMemoryManager :: Session mm mm
getMemoryManager = gets $ view Env.memoryManager


updateMemoryManager :: (mm -> mm) -> Session mm ()
updateMemoryManager updMethod = do
    s <- get
    put $ s { Env._memoryManager = updMethod $ Env._memoryManager s }
-- FIXME[PM] : https://github.com/ekmett/lens/issues/515
--updateMemoryManager updMethod = do
--    s <- get
--    put $ Env.memoryManager %~ updMethod $  s

--updateMemoryManager = modify . over Env.memoryManager
--updateMemoryManager updMethod = Env.memoryManager %= updMethod
--updateMemoryManager updMethod = modify $ Env.memoryManager %~ (updMethod $)
--updateMemoryManager u = modify $ over Env.memoryManager u
--                      = modify $ over Env.memoryManager id


setMemoryManager :: mm -> Session mm ()
setMemoryManager = modify . set Env.memoryManager

---- Env.libManager -------------------------------------------------------

setLibManager :: LibManager -> Session mm ()
setLibManager = modify . set Env.libManager


getLibManager :: Session mm LibManager
getLibManager = gets $ view Env.libManager


getLibrary :: Library.ID -> Session mm Library
getLibrary libraryID = do
    libManager <- getLibManager
    LibManager.lab libManager libraryID
        <??> Error.ASTLookupError $(loc) ("Cannot find library with id=" ++ show libraryID)


getModule :: DefPoint -> Session mm Module
getModule defPoint = do
    focus <- getFocus defPoint
    Focus.getModule focus
        <??> Error.ASTLookupError $(loc) "Target is not a module"


getFunction :: DefPoint -> Session mm Expr
getFunction defPoint = do
    focus <- getFocus defPoint
    Focus.getFunction focus
        <??> Error.ASTLookupError $(loc) "Target is not a function"


getClass :: DefPoint -> Session mm Expr
getClass defPoint = do
    focus <- getFocus defPoint
    Focus.getClass focus
        <??> Error.ASTLookupError $(loc) "Target is not a class"


getFocus :: DefPoint -> Session mm Focus
getFocus (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    hoistEither $ fmapL (Error.ASTLookupError $(loc)) $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast


runPass :: Functor m => Location -> m (Either Error.ErrorStr a) -> EitherT Error m a
runPass loc' p = EitherT $ (fmap . fmapL) (Error.PassError loc') p


getGraph :: DefPoint -> Session mm (Graph, AST.ID)
getGraph defPoint = do
    library     <- getLibrary $ defPoint ^. DefPoint.libraryID
    let propertyMap = library ^. Library.propertyMap
        ast         = library ^. Library.ast
    expr  <- getFunction defPoint
    aa    <- runPass $(loc) $ Alias.run ast
    graph <- fst <$> runPass $(loc) (GraphBuilder.run aa propertyMap False expr)
    return (graph, expr ^. Expr.id)


getPropertyMap :: Library.ID -> Session mm PropertyMap
getPropertyMap libraryID =
    view Library.propertyMap <$> getLibrary libraryID


getFlags :: CallPoint -> Session mm Flags
getFlags callPoint = do
    let libraryID = callPoint ^. CallPoint.libraryID
        nodeID    = callPoint ^. CallPoint.nodeID
    PropertyMap.getFlags nodeID <$> getPropertyMap libraryID


getDefaultsMap :: CallPoint -> Session mm DefaultsMap
getDefaultsMap callPoint = do
    let libraryID = callPoint ^. CallPoint.libraryID
        nodeID    = callPoint ^. CallPoint.nodeID
    PropertyMap.getDefaultsMap nodeID <$> getPropertyMap libraryID

---- Env.projectID --------------------------------------------------------

getProjectID :: Session mm Project.ID
getProjectID = getProjectIDMaybe <??&> Error.ConfigError $(loc) "Project ID not set."


getProjectIDMaybe :: Session mm (Maybe Project.ID)
getProjectIDMaybe = gets (view Env.projectID)


setProjectID :: Project.ID -> Session mm ()
setProjectID = modify . set Env.projectID . Just


unsetProjectID :: Session mm ()
unsetProjectID = modify $ Env.projectID .~ Nothing

---- Env.mainPtr ----------------------------------------------------------

getMainPtr :: Session mm DefPoint
getMainPtr = getMainPtrMaybe <??&> Error.ConfigError $(loc) "MainPtr not set."


getMainPtrMaybe :: Session mm (Maybe DefPoint)
getMainPtrMaybe = gets (view Env.mainPtr)


setMainPtr :: DefPoint -> Session mm ()
setMainPtr mainPtr = modify (Env.mainPtr .~ Just mainPtr)

---- Env.resultCallback ---------------------------------------------------

getResultCallBack :: Session mm Env.ResultCallBack
getResultCallBack = gets $ view Env.resultCallBack

---------------------------------------------------------------------------

whenVisible :: CallPointPath -> Session mm () -> Session mm ()
whenVisible callPointPath action = do
    flags <- getFlags $ last callPointPath
    unless (Flags.isSet' flags (view Flags.defaultNodeGenerated)
         || Flags.isSet' flags (view Flags.graphViewGenerated  )
         || Flags.isFolded flags                               )
        action

---------------------------------------------------------------------------

cleanEnv :: Session mm ()
cleanEnv = cleanWatchPoints
        >> cleanTimeRefs
        >> cleanDependentNodes
        >> cleanProfileInfos
        >> cleanSerializationModes

