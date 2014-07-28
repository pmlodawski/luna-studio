---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Cache.Info where

import Data.Map (Map)

import           Flowbox.Interpreter.Session.Cache.Status (CacheStatus)
import           Flowbox.Interpreter.Session.Data.VarName (VarName)
import qualified Flowbox.Luna.Data.AST.Common             as AST
import           Flowbox.Prelude



data CacheInfo = CacheInfo { _defID         :: AST.ID
                           , _status        :: CacheStatus
                           , _recentVarName :: VarName
                           , _dependencies  :: Map [VarName] VarName
                           } deriving (Show)

makeLenses (''CacheInfo)


--mk :: AST.ID -> VarName -> CacheInfo
--mk defID' recentVarName' = CacheInfo defID' CacheStatus. recentVarName' def
