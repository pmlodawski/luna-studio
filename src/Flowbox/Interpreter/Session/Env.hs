---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Env where


import           Data.Set (Set)
import qualified Data.Set as Set

import           Flowbox.Data.MapTree            (MapForest)
import qualified Flowbox.Data.MapTree            as MapTree
import qualified Flowbox.Interpreter.Mockup.Node as Node
import           Flowbox.Luna.Lib.LibManager     (LibManager)
import           Flowbox.Prelude


data Env = Env { _cached      :: Set       Node.ID
               , _watchPoints :: MapForest Node.ID
               , _libManager  :: LibManager
               } deriving (Show)


makeLenses(''Env)


mk :: LibManager -> Env
mk = Env Set.empty MapTree.empty
