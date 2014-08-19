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
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs  (Breadcrumbs)
import           Flowbox.Prelude



data CacheInfo = CacheInfo { _defID         :: AST.ID
                           , _breadcrumbs   :: Breadcrumbs
                           , _status        :: CacheStatus
                           , _recentVarName :: VarName
                           , _dependencies  :: Map [VarName] VarName
                           } deriving (Show)

makeLenses (''CacheInfo)
