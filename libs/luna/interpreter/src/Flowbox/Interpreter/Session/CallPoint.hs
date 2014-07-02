---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.CallPoint where

import qualified Flowbox.Luna.Data.Graph.Node as Node
import qualified Flowbox.Luna.Lib.Library     as Library



type CallPoint = (Library.ID, Node.ID)

