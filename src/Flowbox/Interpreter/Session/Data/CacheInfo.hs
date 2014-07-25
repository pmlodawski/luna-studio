---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.CacheInfo where

import Data.Hash (Hash)
import Data.Set  (Set)

import qualified Flowbox.Luna.Data.AST.Common as AST
import           Flowbox.Prelude



data CacheInfo = CacheInfo { _defID        :: AST.ID
                           , _modified     :: Bool
                           , _nonCacheable :: Bool
                           , _hashes       :: Set Hash
                           } deriving (Show)

makeLenses (''CacheInfo)


mk :: AST.ID -> CacheInfo
mk defID' = CacheInfo defID' False False def
