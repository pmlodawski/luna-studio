---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Env where

import           Flowbox.Data.MapForest                     (MapForest)
import qualified Flowbox.Data.MapForest                     as MapForest
import           Flowbox.Interpreter.Session.Data.CallPoint (CallPoint)
import           Flowbox.Interpreter.Session.Data.DefPoint  (DefPoint)
import           Flowbox.Luna.Lib.LibManager                (LibManager)
import           Flowbox.Prelude



data Env = Env { _cached      :: MapForest CallPoint
               , _watchPoints :: MapForest CallPoint
               , _libManager  :: LibManager
               , _mainPtr     :: DefPoint
               } deriving (Show)


makeLenses(''Env)


mk :: LibManager -> DefPoint -> Env
mk = Env MapForest.empty MapForest.empty
