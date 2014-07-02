---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.CallPath where

import qualified Data.List as List

import Flowbox.Interpreter.Session.CallPoint (CallPoint)
import Flowbox.Prelude



type CallPath  = [CallPoint]


toVarName :: CallPath -> String
toVarName = List.concat . map gen where
    gen (libraryID, nodeID) = "_" ++ show libraryID ++ "_" ++ show nodeID
