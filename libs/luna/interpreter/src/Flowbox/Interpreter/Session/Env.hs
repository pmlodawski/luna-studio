---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Env where

import           Flowbox.Data.MapForest                  (MapForest)
import qualified Flowbox.Data.MapForest                  as MapForest
import           Flowbox.Interpreter.Session.CallPoint   (CallPoint)
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs (Breadcrumbs)
import           Flowbox.Luna.Lib.LibManager             (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Prelude



data Env = Env { _cached      :: MapForest CallPoint
               , _watchPoints :: MapForest CallPoint
               , _libManager  :: LibManager
               , _mainPtr     :: (Library.ID, Breadcrumbs)
               } deriving (Show)


makeLenses(''Env)


mk :: LibManager -> (Library.ID, Breadcrumbs) -> Env
mk = Env MapForest.empty MapForest.empty
