---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env.Env where

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.Map                (Map)
import           Data.Set                (Set)

import qualified Flowbox.Batch.Project.Project               as Project
import           Flowbox.Data.MapForest                      (MapForest)
import           Flowbox.Data.Mode                           (Mode)
import           Flowbox.Data.SetForest                      (SetForest)
import           Flowbox.Prelude
import           Generated.Proto.Mode.ModeValue              (ModeValue)
import qualified Luna.Graph.Node                             as Node
import           Luna.Interpreter.Session.Cache.Info         (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint     (CallPoint)
import           Luna.Interpreter.Session.Data.CallPointPath (CallPointPath)
import           Luna.Interpreter.Session.Data.DefPoint      (DefPoint)
import qualified Luna.Interpreter.Session.Memory.Config      as Memory
import           Luna.Interpreter.Session.TargetHS.Reload    (ReloadMap)
import           Luna.Lib.Manager                            (LibManager)



type ResultCallBack = Project.ID -> CallPointPath -> [ModeValue] -> IO ()
type FragileMVar    = MVar ()

data Env memoryManager = Env { _cached                   :: MapForest CallPoint CacheInfo
                             , _watchPoints              :: SetForest CallPoint
                             , _reloadMap                :: ReloadMap
                             , _allReady                 :: Bool
                             , _fragileOperation         :: FragileMVar
                             , _dependentNodes           :: Map CallPoint (Set Node.ID)

                             , _defaultSerializationMode :: Mode
                             , _serializationModes       :: MapForest CallPoint (Set Mode)
                             , _memoryConfig             :: Memory.Config
                             , _memoryManager            :: memoryManager

                             , _libManager               :: LibManager
                             , _projectID                :: Maybe Project.ID
                             , _mainPtr                  :: Maybe DefPoint
                             , _resultCallBack           :: ResultCallBack
                             }


makeLenses ''Env


mk :: memoryManager -> LibManager -> Maybe Project.ID -> Maybe DefPoint
   -> ResultCallBack -> IO (Env memoryManager)
mk memoryManager'  libManager' projectID' mainPtr' resultCallBack' = do
    fo <- MVar.newMVar ()
    return $ Env def def def False fo def
                 def def def
                 memoryManager' libManager' projectID' mainPtr' resultCallBack'


mkDef :: memoryManager -> IO (Env memoryManager)
mkDef memoryManager' = mk memoryManager' def def def (const (const (void . return)))
