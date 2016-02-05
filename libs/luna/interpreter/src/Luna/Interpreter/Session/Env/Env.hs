---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.Env where

import           Control.Concurrent.MVar                     (MVar)
import qualified Control.Concurrent.MVar                     as MVar
import           Data.HMap                                   (HMap)
import qualified Data.HMap                                   as HMap
import           Data.IntSet                                 (IntSet)
import           Data.Map                                    (Map)
import           Data.MultiSet                               (MultiSet)
import           Data.Set                                    (Set)
import qualified Language.Preprocessor.Cpphs                 as Cpphs

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Config.Config                       (Config)
import qualified Flowbox.Config.Config                       as Config
import           Flowbox.Data.MapForest                      (MapForest)
import           Flowbox.Data.Mode                           (Mode)
import           Flowbox.Data.SetForest                      (SetForest)
import           Flowbox.Prelude
import           Generated.Proto.Mode.ModeValue              (ModeValue)
import           Luna.DEP.Lib.Manager                        (LibManager)
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.CompiledNode  (CompiledNode)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint)
import           Luna.Interpreter.Session.Data.Time          (Time)
import           Luna.Interpreter.Session.Error              (Error)
import qualified Luna.Interpreter.Session.Memory.Config      as Memory
import           Luna.Interpreter.Session.Profile.Info       (ProfileInfo)
import           Luna.Interpreter.Session.TargetHS.Reload    (ReloadMap)



type ResultCallBack = Project.ID -> CallPointPath -> [ModeValue] -> Time -> IO ()
type FragileMVar    = MVar ()


data Env memoryManager = Env
    { _sessionStatus :: SessionStatus
    , _sessionConfig :: SessionConfig memoryManager
    , _sessionData   :: SessionData
    , _projectData   :: ProjectData
    }

data SessionStatus = SessionStatus
    { _allReady         :: Bool
    , _fragileOperation :: FragileMVar
    }

data SessionConfig memoryManager = SessionConfig
    { _cpphsOptions   :: Cpphs.CpphsOptions
    , _memoryConfig   :: Memory.Config
    , _memoryManager  :: memoryManager
    , _resultCallBack :: ResultCallBack
    }

data SessionData = SessionData
    { _expressions        :: HMap
    , _compiled           :: MapForest CallPoint CompiledNode
    , _cached             :: MapForest CallPoint CacheInfo
    , _watchPoints        :: SetForest CallPoint
    , _reloadMap          :: ReloadMap
    , _dependentNodes     :: Map CallPoint IntSet
    , _timeVar            :: Time
    , _timeRefs           :: Set CallPoint
    , _serializationModes :: MapForest CallPoint (MultiSet Mode)
    , _profileInfos       :: MapForest CallPoint ProfileInfo
    , _compileErrors      :: MapForest CallPoint Error
    }

data ProjectData = ProjectData
    { _projectID  :: Maybe Project.ID
    , _libManager :: LibManager
    , _mainPtr    :: Maybe DefPoint
    }


makeLenses ''Env
makeLenses ''SessionData
makeLenses ''SessionConfig
makeLenses ''SessionStatus
makeLenses ''ProjectData


instance Default SessionData where
    def = SessionData
        {- expressions        -} HMap.empty
        {- compiled           -} def
        {- cached             -} def
        {- watchPoints        -} def
        {- reloadMap          -} def
        {- dependentNodes     -} def
        {- timeVar            -} def
        {- timeRefs           -} def
        {- serializationModes -} def
        {- profileInfos       -} def
        {- compileErrors      -} def

mkSessionStatus :: IO SessionStatus
mkSessionStatus = SessionStatus False <$> MVar.newMVar ()

mkSessionConfig :: Config -> memoryManager -> ResultCallBack -> SessionConfig memoryManager
mkSessionConfig config memoryManager' resultCallBack' =
    SessionConfig (mkCpphsOptions config)
                  def
                  memoryManager'
                  resultCallBack'

mkCpphsOptions :: Config -> Cpphs.CpphsOptions
mkCpphsOptions config = Cpphs.CpphsOptions [] [] [] []
                            [Config.path (Config.templates config) ++ "/pragmas.h"]
                            Cpphs.defaultBoolOptions { Cpphs.locations = False }



mk :: Config -> memoryManager -> LibManager -> Maybe Project.ID -> Maybe DefPoint
   -> ResultCallBack -> IO (Env memoryManager)
mk config memoryManager' libManager' projectID' mainPtr' resultCallBack' =
    Env <$> mkSessionStatus
        <*> pure (mkSessionConfig config memoryManager' resultCallBack')
        <*> pure def
        <*> pure (ProjectData projectID' libManager' mainPtr')


mkDef :: Config -> memoryManager -> IO (Env memoryManager)
mkDef config memoryManager' = mk config
            {- memoryManager  -} memoryManager'
            {- libManager     -} def
            {- projectID      -} def
            {- mainPtr        -} def
            {- resultCallBack -} (const (const (const (void . return))))
