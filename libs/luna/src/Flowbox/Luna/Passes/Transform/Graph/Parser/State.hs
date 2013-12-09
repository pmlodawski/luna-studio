---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Flowbox.Luna.Passes.Transform.Graph.Parser.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Debug.Trace
import           Flowbox.Luna.Data.AliasAnalysis (AA)
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import qualified Flowbox.Luna.Data.AST.Utils     as AST
import           Flowbox.Luna.Data.Graph.Edge    (Edge)
import           Flowbox.Luna.Data.Graph.Graph   (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph   as Graph
import           Flowbox.Luna.Data.Graph.Node    (Node)
import qualified Flowbox.Luna.Data.Graph.Node    as Node
import           Flowbox.Luna.Data.Graph.Port    (OutPort)
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Transform.Graph.Parser.State"


type NodeMap = Map AST.ID (Node.ID, OutPort)


data GPState = GPState deriving (Show)


type GPStateM m = MonadState GPState m


empty :: GPState
empty = GPState

