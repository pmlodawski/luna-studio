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

import qualified Flowbox.Interpreter.Mockup.Node as Node
import           Flowbox.Prelude



data Env = Env { _cached :: Set Node.ID
               } deriving (Read, Show)


makeLenses(''Env)


empty :: Env
empty = Env Set.empty
