---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.CacheInfo where

import qualified Flowbox.Luna.Data.AST.Common as AST
import           Flowbox.Prelude



data CacheInfo = CacheInfo { _defID :: AST.ID
                           } deriving (Show)

makeLenses (''CacheInfo)


