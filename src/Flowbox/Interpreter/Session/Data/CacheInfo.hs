---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Interpreter.Session.Data.CacheInfo where

import Data.Set (Set)

import qualified Flowbox.Luna.Data.AST.Common as AST
import           Flowbox.Prelude



type Hash = Int

data CacheInfo = CacheInfo { _defID :: AST.ID
                           --, _modified  :: Bool
                           --, _cacheable :: Bool
                           --, _hashes    :: Set Hash
                           } deriving (Show)

makeLenses (''CacheInfo)


