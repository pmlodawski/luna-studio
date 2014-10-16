---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Graph.Properties where

import Flowbox.Prelude
import Luna.Graph.Attributes               (Attributes)
import Luna.Graph.Flags                    (Flags)
import Luna.Graph.View.Default.DefaultsMap (DefaultsMap)



data Properties = Properties { _flags       :: Flags
                             , _defaultsMap :: DefaultsMap
                             , _attrs       :: Attributes
                             } deriving (Show, Read, Eq)

makeLenses ''Properties


instance Default Properties where
    def = Properties def def def
