---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.CallPath where

import Flowbox.Interpreter.Session.CallPoint (CallPoint)
import Flowbox.Prelude



type CallPath  = [CallPoint]


toVarName :: CallPath -> String
toVarName callPath = '_' : toVarName' callPath where
    toVarName' []        = ""
    toVarName' ((a,b):t) = show a ++ "_" ++ show b ++ toVarName' t

