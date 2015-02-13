---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.Env where

import           Control.Concurrent.MVar     (MVar)
import qualified Control.Concurrent.MVar     as MVar
import           Data.IntSet                 (IntSet)
import           Data.Map                    (Map)
import           Data.MultiSet               (MultiSet)
import           Data.Set                    (Set)
import qualified Language.Preprocessor.Cpphs as Cpphs

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
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint)
import qualified Luna.Interpreter.Session.Memory.Config      as Memory
import           Luna.Interpreter.Session.ProfileInfo        (ProfileInfo)
import           Luna.Interpreter.Session.TargetHS.Reload    (ReloadMap)



type ResultCallBack = Project.ID -> CallPointPath -> [ModeValue] -> IO ()
type FragileMVar    = MVar ()
type TimeVar        = Float

data Env memoryManager = Env { _cached             :: MapForest CallPoint CacheInfo
                             , _watchPoints        :: SetForest CallPoint
                             , _reloadMap          :: ReloadMap
                             , _allReady           :: Bool
                             , _fragileOperation   :: FragileMVar
                             , _dependentNodes     :: Map CallPoint IntSet
                             , _profileInfos       :: MapForest CallPoint ProfileInfo
                             , _cpphsOptions       :: Cpphs.CpphsOptions

                             , _timeVar            :: TimeVar
                             , _timeRefs           :: Set CallPoint

                             , _serializationModes :: MapForest CallPoint (MultiSet Mode)
                             , _memoryConfig       :: Memory.Config
                             , _memoryManager      :: memoryManager

                             , _libManager         :: LibManager
                             , _projectID          :: Maybe Project.ID
                             , _mainPtr            :: Maybe DefPoint
                             , _resultCallBack     :: ResultCallBack
                             }


makeLenses ''Env


mkCpphsOptions :: Config -> Cpphs.CpphsOptions
mkCpphsOptions config = Cpphs.CpphsOptions [] [] [] []
                            [Config.path (Config.templates config) ++ "/pragmas.h"]
                            Cpphs.defaultBoolOptions { Cpphs.locations = False }


mk :: Config -> memoryManager -> LibManager -> Maybe Project.ID -> Maybe DefPoint
   -> ResultCallBack -> IO (Env memoryManager)
mk config memoryManager'  libManager' projectID' mainPtr' resultCallBack' = do
    fo <- MVar.newMVar ()
    return $ Env def def def False fo def def (mkCpphsOptions config)
                 def def
                 def def memoryManager'
                 libManager' projectID' mainPtr' resultCallBack'


mkDef :: Config -> memoryManager -> IO (Env memoryManager)
mkDef config memoryManager' = mk config memoryManager' def def def (const (const (void . return)))
