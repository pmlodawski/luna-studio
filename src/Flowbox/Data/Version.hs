---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Data.Version where

import           Flowbox.Prelude   
import           GHC.Generics      
import           Data.Aeson        
import           Data.Monoid       (Monoid, mempty)

data Version = Version { branch :: [Int]
                       , tags   :: [String]
                       } deriving (Show, Eq, Generic)

-------------------------------------------------
-- INSTANCES
-------------------------------------------------

instance Monoid Version where
    mempty = Version { branch = [0,1,0]
                     , tags   = mempty
                     }

instance ToJSON Version
instance FromJSON Version

