---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.State where

import qualified Control.Monad.Ghc          as MGHC
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Data.Monoid                ((<>))

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Control.Error
import           Flowbox.Data.MapForest                      (MapForest)
import qualified Flowbox.Data.MapForest                      as MapForest
import           Flowbox.Data.Mode                           (Mode)
import           Flowbox.Prelude
import           Flowbox.Source.Location                     (Location, loc)
import           Generated.Proto.Data.Value                  (Value)
import qualified Luna.AST.Common                             as AST
import           Luna.AST.Control.Focus                      (Focus)
import qualified Luna.AST.Control.Focus                      as Focus
import qualified Luna.AST.Control.Zipper                     as Zipper
import           Luna.AST.Expr                               (Expr)
import qualified Luna.AST.Expr                               as Expr
import           Luna.AST.Module                             (Module)
import           Luna.Graph.Flags                            (Flags)
import           Luna.Graph.Graph                            (Graph)
import           Luna.Graph.PropertyMap                      (PropertyMap)
import qualified Luna.Graph.PropertyMap                      as PropertyMap
import           Luna.Graph.View.Default.DefaultsMap         (DefaultsMap)
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import qualified Luna.Interpreter.Session.Data.CallPoint     as CallPoint
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint (DefPoint))
import qualified Luna.Interpreter.Session.Data.DefPoint      as DefPoint
import           Luna.Interpreter.Session.Env.Env            (Env)
import qualified Luna.Interpreter.Session.Env.Env            as Env
import           Luna.Interpreter.Session.Error              (Error)
import qualified Luna.Interpreter.Session.Error              as Error
import           Luna.Interpreter.Session.TargetHS.Reload    (Reload, ReloadMap)
import           Luna.Lib.Lib                                (Library)
import qualified Luna.Lib.Lib                                as Library
import           Luna.Lib.Manager                            (LibManager)
import qualified Luna.Lib.Manager                            as LibManager
import qualified Luna.Pass.Analysis.Alias.Alias              as Alias
import qualified Luna.Pass.Transform.Graph.Builder.Builder   as GraphBuilder



type SessionST = StateT Env MGHC.Ghc

type Session = EitherT Error SessionST


---- Env.cached -----------------------------------------------------------

getCached :: Session (MapForest CallPoint CacheInfo)
getCached = gets (view Env.cached)


cachedInsert :: CallPointPath -> CacheInfo -> Session ()
cachedInsert callPointPath cacheInfo =
    modify (Env.cached %~ MapForest.insert callPointPath cacheInfo)


cachedDelete :: CallPointPath -> Session ()
cachedDelete callPointPath =
    modify $ Env.cached %~ MapForest.delete callPointPath


cachedLookup :: CallPointPath -> Session (Maybe CacheInfo)
cachedLookup callPointPath = MapForest.lookup callPointPath <$> getCached

---- Env.watchPoints ------------------------------------------------------
---- Env.reloadMap --------------------------------------------------------

addReload :: Library.ID -> Reload -> Session ()
addReload libraryID reload = modify (Env.reloadMap %~ update) where
    update = Map.alter (Just . (<> reload) . Maybe.fromMaybe def) libraryID


getReloads :: Session ReloadMap
getReloads = gets $ view Env.reloadMap


cleanReloads :: Session ()
cleanReloads = modify (Env.reloadMap .~ mempty)

---- Env.allReady ---------------------------------------------------------

setAllReady :: Bool -> Session ()
setAllReady flag = modify $ Env.allReady .~ flag


getAllReady :: Session Bool
getAllReady = gets $ view Env.allReady

---- Env.defaultSerializationMode -----------------------------------------

getDefaultSerializationMode :: Session Mode
getDefaultSerializationMode = gets $ view Env.defaultSerializationMode


setDefaultSerializationMode :: Mode -> Session ()
setDefaultSerializationMode mode =
    modify (Env.defaultSerializationMode .~ mode)

---- Env.serializationModes -----------------------------------------------

getSerializationModes :: Session (MapForest CallPoint Mode)
getSerializationModes = gets $ view Env.serializationModes


lookupSerializationMode :: CallPointPath -> Session (Maybe Mode)
lookupSerializationMode callPointPath =
    MapForest.lookup callPointPath <$> getSerializationModes


getSerializationMode :: CallPointPath -> Session Mode
getSerializationMode callPointPath = do
    mode <- lookupSerializationMode callPointPath
    Maybe.maybe getDefaultSerializationMode return mode


insertSerializationMode :: CallPointPath -> Mode -> Session ()
insertSerializationMode callPointPath mode =
    modify (Env.serializationModes %~ MapForest.insert callPointPath mode)


deleteSerializationMode :: CallPointPath -> Session ()
deleteSerializationMode callPointPath =
    modify (Env.serializationModes %~ MapForest.delete callPointPath)

---- Env.libManager -------------------------------------------------------

setLibManager :: LibManager -> Session ()
setLibManager libManager = modify $ Env.libManager .~ libManager


getLibManager :: Session LibManager
getLibManager = gets $ view Env.libManager


getLibrary :: Library.ID -> Session Library
getLibrary libraryID = do
    libManager <- getLibManager
    LibManager.lab libManager libraryID
        <??> Error.ASTLookupError $(loc) ("Cannot find library with id=" ++ show libraryID)


getModule :: DefPoint -> Session Module
getModule defPoint = do
    focus <- getFocus defPoint
    Focus.getModule focus
        <??> Error.ASTLookupError $(loc) "Target is not a module"


getFunction :: DefPoint -> Session Expr
getFunction defPoint = do
    focus <- getFocus defPoint
    Focus.getFunction focus
        <??> Error.ASTLookupError $(loc) "Target is not a function"


getClass :: DefPoint -> Session Expr
getClass defPoint = do
    focus <- getFocus defPoint
    Focus.getClass focus
        <??> Error.ASTLookupError $(loc) "Target is not a class"


getFocus :: DefPoint -> Session Focus
getFocus (DefPoint libraryID bc) = do
    ast <- view Library.ast <$> getLibrary libraryID
    hoistEither $ fmapL (Error.ASTLookupError $(loc)) $ Zipper.getFocus <$> Zipper.focusBreadcrumbs' bc ast


runPass :: Functor m => Location -> m (Either Error.ErrorStr a) -> EitherT Error m a
runPass loc' p = EitherT $ (fmap . fmapL) (Error.PassError loc') p


getGraph :: DefPoint -> Session (Graph, AST.ID)
getGraph defPoint = do
    library     <- getLibrary $ defPoint ^. DefPoint.libraryID
    let propertyMap = library ^. Library.propertyMap
        ast         = library ^. Library.ast
    expr  <- getFunction defPoint
    aa    <- runPass $(loc) $ Alias.run ast
    graph <- fst <$> runPass $(loc) (GraphBuilder.run aa propertyMap False expr)
    return (graph, expr ^. Expr.id)


getPropertyMap :: Library.ID -> Session PropertyMap
getPropertyMap libraryID =
    view Library.propertyMap <$> getLibrary libraryID


getFlags :: CallPoint -> Session Flags
getFlags callPiont = do
    let libraryID = callPiont ^. CallPoint.libraryID
        nodeID    = callPiont ^. CallPoint.nodeID
    PropertyMap.getFlags nodeID <$> getPropertyMap libraryID


getDefaultsMap :: CallPoint -> Session DefaultsMap
getDefaultsMap callPiont = do
    let libraryID = callPiont ^. CallPoint.libraryID
        nodeID    = callPiont ^. CallPoint.nodeID
    PropertyMap.getDefaultsMap nodeID <$> getPropertyMap libraryID

---- Env.projectID --------------------------------------------------------

getProjectID :: Session Project.ID
getProjectID = getProjectIDMaybe <??&> Error.ConfigError $(loc) "Project ID not set."


getProjectIDMaybe :: Session (Maybe Project.ID)
getProjectIDMaybe = gets (view Env.projectID)


setProjectID :: Project.ID -> Session ()
setProjectID projectID = modify (Env.projectID .~ Just projectID)

---- Env.mainPtr ----------------------------------------------------------

getMainPtr :: Session DefPoint
getMainPtr = getMainPtrMaybe <??&> Error.ConfigError $(loc) "MainPtr not set."


getMainPtrMaybe :: Session (Maybe DefPoint)
getMainPtrMaybe = gets (view Env.mainPtr)


setMainPtr :: DefPoint -> Session ()
setMainPtr mainPtr = modify (Env.mainPtr .~ Just mainPtr)

---- Env.resultCallback ---------------------------------------------------

getResultCallBack :: Session (Project.ID -> CallPointPath -> Maybe Value -> IO ())
getResultCallBack = gets $ view Env.resultCallBack
