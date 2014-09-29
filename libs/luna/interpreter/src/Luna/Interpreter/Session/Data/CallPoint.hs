---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Data.CallPoint where

import           Flowbox.Prelude
import qualified Luna.Graph.Node as Node
import qualified Luna.Lib.Lib    as Library



data CallPoint = CallPoint { _libraryID :: Library.ID
                           , _nodeID    :: Node.ID
                           } deriving (Show, Ord, Eq)

makeLenses (''CallPoint)

