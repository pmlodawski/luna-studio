---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.State where

import qualified Control.Concurrent.MVar                       as MVar
import           Control.Monad.Catch                           (bracket_)
import           Control.Monad.State
import           Control.Monad.Trans.Either
import           Data.HMap                                     (HMap)
import           Data.IntSet                                   (IntSet)
import qualified Data.IntSet                                   as IntSet
import           Data.Map                                      (Map)
import qualified Data.Map                                      as Map
import qualified Data.Maybe                                    as Maybe
import           Data.MultiSet                                 (MultiSet)
import qualified Data.MultiSet                                 as MultiSet
import           Data.Set                                      (Set)
import qualified Data.Set                                      as Set
import qualified Language.Preprocessor.Cpphs                   as Cpphs

import qualified Flowbox.Batch.Project.Project                 as Project
import           Flowbox.Control.Error                         hiding (err)
import           Flowbox.Data.MapForest                        (MapForest)
import qualified Flowbox.Data.MapForest                        as MapForest
import           Flowbox.Data.Mode                             (Mode)
import           Flowbox.Data.SetForest                        (SetForest)
import qualified Flowbox.Data.SetForest                        as SetForest
import           Flowbox.Prelude
import           Flowbox.Source.Location                       (Location, loc)
import qualified Luna.DEP.AST.AST                              as AST
import           Luna.DEP.AST.Control.Focus                    (Focus)
import qualified Luna.DEP.AST.Control.Focus                    as Focus
import qualified Luna.DEP.AST.Control.Zipper                   as Zipper
import           Luna.DEP.AST.Expr                             (Expr)
import qualified Luna.DEP.AST.Expr                             as Expr
import           Luna.DEP.AST.Module                           (Module)
import           Luna.DEP.Graph.Flags                          (Flags)
import qualified Luna.DEP.Graph.Flags                          as Flags
import           Luna.DEP.Graph.Graph                          (Graph)
import qualified Luna.DEP.Graph.Node                           as Node
import           Luna.DEP.Graph.PropertyMap                    (PropertyMap)
import qualified Luna.DEP.Graph.PropertyMap                    as PropertyMap
import           Luna.DEP.Graph.View.Default.DefaultsMap       (DefaultsMap)
import           Luna.DEP.Lib.Lib                              (Library)
import qualified Luna.DEP.Lib.Lib                              as Library
import           Luna.DEP.Lib.Manager                          (LibManager)
import qualified Luna.DEP.Lib.Manager                          as LibManager
import qualified Luna.DEP.Pass.Analysis.Alias.Alias            as Alias
import qualified Luna.DEP.Pass.Transform.Graph.Builder.Builder as GraphBuilder
import           Luna.Interpreter.Session.Cache.Info           (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint       (CallPoint)
import qualified Luna.Interpreter.Session.Data.CallPoint       as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath   (CallPointPath)
import           Luna.Interpreter.Session.Data.CompiledNode    (CompiledNode)
import           Luna.Interpreter.Session.Data.DefPoint        (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Data.DefPoint        as DefPoint
import           Luna.Interpreter.Session.Data.KeyName         (KeyName)
import qualified Luna.Interpreter.Session.Data.KeyName         as KeyName
import           Luna.Interpreter.Session.Data.Time            (Time)
import qualified Luna.Interpreter.Session.Env.Env              as Env
import           Luna.Interpreter.Session.Env.Session          (Session)
import           Luna.Interpreter.Session.Error                (Error)
import qualified Luna.Interpreter.Session.Error                as Error
import qualified Luna.Interpreter.Session.Memory.Config        as Memory
import           Luna.Interpreter.Session.Profile.Info         (ProfileInfo)
import           Luna.Interpreter.Session.TargetHS.Reload      (Reload, ReloadMap)



---- Env.sessiondata.expressions ------------------------------------------

getExpressions :: Session mm HMap
getExpressions = gets $ view $ Env.sessionData . Env.expressions

setExpressions :: HMap -> Session mm ()
setExpressions = modify . set (Env.sessionData . Env.expressions)

updateExpressions :: (HMap -> HMap) -> Session mm ()
updateExpressions = modify . over (Env.sessionData . Env.expressions)

---- Env.compiled ---------------------------------------------------------

getCompiled :: Session mm (MapForest CallPoint CompiledNode)
getCompiled = gets $ view $ Env.sessionData . Env.compiled

compiledInsert :: CallPointPath -> CompiledNode -> Session mm ()
compiledInsert = (modify . over (Env.sessionData . Env.compiled)) .: MapForest.insert

compiledDelete :: CallPointPath -> Session mm ()
compiledDelete = modify . over (Env.sessionData . Env.compiled) . MapForest.delete

compiledLookup :: CallPointPath -> Session mm (Maybe CompiledNode)
compiledLookup callPath = MapForest.lookup callPath <$> getCompiled

compiledClean :: Session mm ()
compiledClean = modify $ Env.sessionData . Env.compiled .~ def

---- Env.cached -----------------------------------------------------------

getCached :: Session mm (MapForest CallPoint CacheInfo)
getCached = gets $ view $ Env.sessionData . Env.cached


cachedInsert :: CallPointPath -> CacheInfo -> Session mm ()
cachedInsert = (modify . over (Env.sessionData . Env.cached)) .: MapForest.insert


cachedDelete :: CallPointPath -> Session mm ()
cachedDelete = modify . over (Env.sessionData . Env.cached) . MapForest.delete


cachedLookup :: CallPointPath -> Session mm (Maybe CacheInfo)
cachedLookup callPointPath = MapForest.lookup callPointPath <$> getCached


cachedClear :: Session mm ()
cachedClear = modify (Env.sessionData . Env.cached .~ def)

---- Env.watchPoints ------------------------------------------------------

addWatchPoint :: CallPointPath -> Session mm ()
addWatchPoint callPath = modify (Env.sessionData . Env.watchPoints %~ SetForest.insert callPath)


deleteWatchPoint :: CallPointPath -> Session mm ()
deleteWatchPoint callPath = modify (Env.sessionData . Env.watchPoints %~ SetForest.delete callPath)


cleanWatchPoints :: Session mm ()
cleanWatchPoints = modify (Env.sessionData . Env.watchPoints .~ def)


getWatchPoints :: Session mm (SetForest CallPoint)
getWatchPoints = gets $ view $ Env.sessionData . Env.watchPoints

---- Env.reloadMap --------------------------------------------------------

addReload :: Library.ID -> Reload -> Session mm ()
addReload libraryID reload = modify (Env.sessionData . Env.reloadMap %~ update) where
    update = Map.alter (Just . (<> reload) . Maybe.fromMaybe def) libraryID


getReloads :: Session mm ReloadMap
getReloads = gets $ view $ Env.sessionData . Env.reloadMap


cleanReloads :: Session mm ()
cleanReloads = modify (Env.sessionData . Env.reloadMap .~ mempty)

---- Env.allReady ---------------------------------------------------------

setAllReady :: Bool -> Session mm ()
setAllReady = modify . set (Env.sessionStatus . Env.allReady)


getAllReady :: Session mm Bool
getAllReady = gets $ view $ Env.sessionStatus . Env.allReady

---- Env.fragileOperation -------------------------------------------------

getFragile :: Session mm Env.FragileMVar
getFragile = gets $ view $ Env.sessionStatus . Env.fragileOperation


fragile :: Session mm a -> Session mm a
fragile action = do
    f <- getFragile
    lift (bracket_ (liftIO $ MVar.takeMVar f) (liftIO $ MVar.putMVar f ()) $ runEitherT action) >>= hoistEither

---- Env.dependentNodes ---------------------------------------------------

getDependentNodes :: Session mm (Map CallPoint IntSet)
getDependentNodes = gets $ view $ Env.sessionData . Env.dependentNodes


getDependentNodesOf :: CallPoint -> Session mm IntSet
getDependentNodesOf callPoint =
    Maybe.fromMaybe def . Map.lookup callPoint <$> getDependentNodes


insertDependentNode :: CallPoint -> Node.ID -> Session mm ()
insertDependentNode callPoint nodeID =
    modify (Env.sessionData . Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.insert nodeID . Maybe.fromMaybe def


insertDependentNodes :: CallPoint -> IntSet -> Session mm ()
insertDependentNodes callPoint nodeIDs =
    modify (Env.sessionData . Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.union nodeIDs . Maybe.fromMaybe def


deleteDependentNodes :: CallPoint -> Session mm ()
deleteDependentNodes = modify . over (Env.sessionData . Env.dependentNodes) . Map.delete


deleteDependentNode :: CallPoint -> Node.ID -> Session mm ()
deleteDependentNode callPoint nodeID =
    modify (Env.sessionData . Env.dependentNodes %~ Map.alter alter callPoint) where
        alter = Just . IntSet.delete nodeID . Maybe.fromMaybe def


cleanDependentNodes :: Session mm ()
cleanDependentNodes = modify (Env.sessionData . Env.dependentNodes .~ def)

---- Env.cpphsOptions -----------------------------------------------------

getCpphsOptions :: Session mm Cpphs.CpphsOptions
getCpphsOptions = gets $ view $ Env.sessionConfig . Env.cpphsOptions

---- Env.profileInfos -----------------------------------------------------

cleanProfileInfos :: Session mm ()
cleanProfileInfos = modify $ Env.sessionData . Env.profileInfos .~ def


getProfileInfos :: Session mm (MapForest CallPoint ProfileInfo)
getProfileInfos = gets $ view $ Env.sessionData . Env.profileInfos


mergeProfileInfo :: CallPointPath -> ProfileInfo -> Session mm ()
mergeProfileInfo callPointPath info =
    modify (Env.sessionData . Env.profileInfos %~ MapForest.alter mergeInfo callPointPath) where
        mergeInfo Nothing  = Just info
        mergeInfo (Just i) = Just $ i <> info

---- Env.compileErrors ----------------------------------------------------

cleanCompileErrors :: Session mm ()
cleanCompileErrors = modify $ Env.sessionData . Env.compileErrors .~ def


getCompileErrors :: Session mm (MapForest CallPoint Error)
getCompileErrors = gets $ view $ Env.sessionData . Env.compileErrors


insertCompileError :: CallPointPath -> Error -> Session mm ()
insertCompileError callPointPath err =
    modify (Env.sessionData . Env.compileErrors %~ MapForest.insert callPointPath err)


reportCompileErrors :: CallPointPath -> Session mm () -> Session mm ()
reportCompileErrors callPointPath action =
    lift (runEitherT action) >>= \case
        Left err -> whenVisible callPointPath $ insertCompileError callPointPath err
        Right () -> return ()

---- Env.timeVar ----------------------------------------------------------

getTimeVar :: Session mm Time
getTimeVar = gets $ view $ Env.sessionData . Env.timeVar


setTimeVar :: Time -> Session mm ()
setTimeVar = modify . set (Env.sessionData . Env.timeVar)

---- Env.timeRefs ---------------------------------------------------------

insertTimeRef :: CallPoint -> Session mm ()
insertTimeRef callPoint = modify (Env.sessionData . Env.timeRefs %~ Set.insert callPoint)


deleteTimeRef :: CallPoint -> Session mm ()
deleteTimeRef callPoint = modify (Env.sessionData . Env.timeRefs %~ Set.delete callPoint)


getTimeRefs :: Session mm (Set CallPoint)
getTimeRefs = gets $ view $ Env.sessionData . Env.timeRefs


cleanTimeRefs :: Session mm ()
cleanTimeRefs = modify $ Env.sessionData . Env.timeRefs .~ def

---- Env.serializationModes -----------------------------------------------

getSerializationModesMap :: Session mm (MapForest CallPoint (MultiSet Mode))
getSerializationModesMap = gets $ view $ Env.sessionData . Env.serializationModes


lookupSerializationModes :: CallPointPath -> Session mm (Maybe (MultiSet Mode))
lookupSerializationModes callPointPath =
    MapForest.lookup callPointPath <$> getSerializationModesMap


getSerializationModes :: CallPointPath -> Session mm (MultiSet Mode)
getSerializationModes callPointPath =
    Maybe.fromMaybe MultiSet.empty <$> lookupSerializationModes callPointPath


insertSerializationModes :: CallPointPath -> MultiSet Mode -> Session mm ()
insertSerializationModes callPointPath modes =
    modify (Env.sessionData . Env.serializationModes %~ MapForest.alter ins callPointPath) where
        ins  Nothing = Just modes
        ins (Just s) = Just $ MultiSet.union s modes


deleteSerializationModes :: CallPointPath -> MultiSet Mode -> Session mm ()
deleteSerializationModes callPointPath modes =
    modify (Env.sessionData . Env.serializationModes %~ MapForest.alter del callPointPath) where
        del  Nothing = Just modes
        del (Just s) = Just $ MultiSet.difference s modes


deleteAllSerializationModes :: CallPointPath -> Session mm ()
deleteAllSerializationModes = modify . over (Env.sessionData . Env.serializationModes) . MapForest.delete


cleanSerializationModes :: Session mm ()
cleanSerializationModes = modify (Env.sessionData . Env.serializationModes .~ def)

---- Env.memoryConfig -----------------------------------------------------

getMemoryConfig :: Session mm Memory.Config
getMemoryConfig = gets $ view $ Env.sessionConfig . Env.memoryConfig


setMemoryConfig :: Memory.Config -> Session mm ()
setMemoryConfig = modify . set (Env.sessionConfig . Env.memoryConfig)

---- Env.memoryManager ----------------------------------------------------

getMemoryManager :: Session mm mm
getMemoryManager = gets $ view $ Env.sessionConfig . Env.memoryManager


updateMemoryManager :: (mm -> mm) -> Session mm ()
updateMemoryManager updMethod = do
    s <- get
    let sessionConfig  = Env._sessionConfig s
        sessionConfig' = sessionConfig { Env._memoryManager = updMethod $ Env._memoryManager sessionConfig}
    put $ s { Env._sessionConfig = sessionConfig' }
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
setMemoryManager = modify . set (Env.sessionConfig . Env.memoryManager)

---- Env.libManager -------------------------------------------------------

setLibManager :: LibManager -> Session mm ()
setLibManager = modify . set (Env.projectData . Env.libManager)


getLibManager :: Session mm LibManager
getLibManager = gets $ view $ Env.projectData . Env.libManager


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
    graph <- fst <$> runPass $(loc) (GraphBuilder.run aa propertyMap True expr)
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
getProjectIDMaybe = gets $ view $ Env.projectData . Env.projectID


setProjectID :: Project.ID -> Session mm ()
setProjectID = modify . set (Env.projectData . Env.projectID) . Just


unsetProjectID :: Session mm ()
unsetProjectID = modify $ Env.projectData . Env.projectID .~ Nothing

---- Env.mainPtr ----------------------------------------------------------

getMainPtr :: Session mm DefPoint
getMainPtr = getMainPtrMaybe <??&> Error.ConfigError $(loc) "MainPtr not set."


getMainPtrMaybe :: Session mm (Maybe DefPoint)
getMainPtrMaybe = gets $ view $ Env.projectData . Env.mainPtr


setMainPtr :: DefPoint -> Session mm ()
setMainPtr mainPtr = modify (Env.projectData . Env.mainPtr .~ Just mainPtr)

---- Env.resultCallback ---------------------------------------------------

getResultCallBack :: Session mm Env.ResultCallBack
getResultCallBack = gets $ view $ Env.sessionConfig . Env.resultCallBack

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
cleanEnv = modify $ Env.sessionData .~ def


--FIXME[PM] Ugly workarounds ----------------
keyNameToString' :: KeyName -> Session mm (Bool, String)
keyNameToString' keyName = do
    let callPoint = last $ keyName ^. KeyName.callPointPath
    moriginID <- view Flags.defaultNodeOriginID <$> getFlags callPoint
    return $ case moriginID of
                 Just originID -> (originID == callPoint ^. CallPoint.nodeID, "_" <> show originID)
                 Nothing       -> (True, KeyName.toString keyName)

keyNameToString :: KeyName -> Session mm String
keyNameToString = fmap snd . keyNameToString'
----------------------------------------------
