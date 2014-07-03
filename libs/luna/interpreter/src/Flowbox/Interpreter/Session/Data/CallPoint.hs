---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.CallPoint where

import qualified Flowbox.Luna.Data.Graph.Node as Node
import qualified Flowbox.Luna.Lib.Library     as Library
import           Flowbox.Prelude



data CallPoint = CallPoint { _libraryID :: Library.ID
                           , _nodeID    :: Node.ID
                           } deriving (Show, Ord, Eq)

makeLenses (''CallPoint)

