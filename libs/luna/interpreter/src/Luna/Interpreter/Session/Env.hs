---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Env where

import qualified Flowbox.Batch.Project.Project           as Project
import           Flowbox.Data.MapForest                  (MapForest)
import qualified Flowbox.Data.MapForest                  as MapForest
import           Flowbox.Data.SetForest                  (SetForest)
import qualified Flowbox.Data.SetForest                  as SetForest
import           Flowbox.Prelude
import qualified Luna.AST.Control.Crumb                  as Crumb
import           Luna.Interpreter.Session.Cache.Info     (CacheInfo)
import           Luna.Interpreter.Session.Data.CallPoint (CallPoint)
import           Luna.Interpreter.Session.Data.DefPoint  (DefPoint (DefPoint))
import           Luna.Lib.Manager                        (LibManager)
import qualified Luna.Lib.Manager                        as LibManager



data Env = Env { _cached      :: MapForest CallPoint CacheInfo
               , _watchPoints :: SetForest CallPoint
               , _libManager  :: LibManager
               , _projectID   :: Project.ID
               , _mainPtr     :: DefPoint
               } deriving (Show)


makeLenses(''Env)


mk :: LibManager -> Project.ID -> DefPoint -> Env
mk = Env MapForest.empty SetForest.empty


instance Default Env where
    def = mk LibManager.empty
             0
             (DefPoint 0 [Crumb.Module "Main", Crumb.Function "main" []])
