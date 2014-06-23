
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Mockup.Graph
( module X
, ID
, Code
, CodeGraph
, Dependency(..)
)
where

import Flowbox.Data.Graph as X
import Flowbox.Prelude



type ID   = Int


type Code = String
data Dependency = Dependency deriving (Read, Show)


type CodeGraph = Graph Code Dependency
