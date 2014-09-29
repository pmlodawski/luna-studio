---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Cache.Info where

import Data.Map (Map)

import           Flowbox.Prelude
import qualified Luna.AST.Common                       as AST
import           Luna.AST.Control.Crumb                (Breadcrumbs)
import           Luna.Interpreter.Session.Cache.Status (CacheStatus)
import           Luna.Interpreter.Session.Data.VarName (VarName)



data CacheInfo = CacheInfo { _defID         :: AST.ID
                           , _breadcrumbs   :: Breadcrumbs
                           , _status        :: CacheStatus
                           , _recentVarName :: VarName
                           , _dependencies  :: Map [VarName] VarName
                           } deriving (Show)

makeLenses ''CacheInfo
