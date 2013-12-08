---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Distribution.License where

import Flowbox.Data.Version (Version)
import Flowbox.Prelude

import Data.Aeson
import Data.Default (Default, def)
import GHC.Generics


data License = GPL (Maybe Version)
             | AGPL (Maybe Version)
             | LGPL (Maybe Version)
             | BSD3
             | MIT
             | Apache (Maybe Version)
             | PublicDomain
             | AllRightsReserved
             | OtherLicense String
             | UnknownLicense 
             deriving (Read, Show, Eq, Generic)

-------------------------------------------------
-- INSTANCES
-------------------------------------------------

instance Default License where
    def = AllRightsReserved

instance ToJSON License
instance FromJSON License
