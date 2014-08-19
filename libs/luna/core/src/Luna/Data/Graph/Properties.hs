---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Graph.Properties where

import           Luna.Data.Graph.Attributes  (Attributes)
import           Luna.Data.Graph.Flags (Flags)
import           Flowbox.Prelude


data Properties = Properties { _flags :: Flags
                             , _attrs :: Attributes
                             } deriving (Show, Read, Eq)

makeLenses (''Properties)


instance Default Properties where
    def = Properties def def