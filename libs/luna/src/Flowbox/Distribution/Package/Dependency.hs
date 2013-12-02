---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.Package.Dependency where

import           Flowbox.Prelude   
import           Data.Version      (Version)
import qualified Data.Version    as Version
import           GHC.Generics      

data Dependency = Dependency { name    :: String
                             --, version :: CVersion
                             } deriving (Show, Generic)

