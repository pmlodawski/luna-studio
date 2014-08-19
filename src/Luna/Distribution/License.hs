---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Luna.Distribution.License where

import Flowbox.Data.Version (Version)
import Flowbox.Prelude

import Data.Aeson
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
             deriving (Read, Show, Eq, Generic, Ord)

-------------------------------------------------
-- INSTANCES
-------------------------------------------------

instance Default License where
    def = AllRightsReserved

instance ToJSON License
instance FromJSON License
