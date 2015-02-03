---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.DEP.Graph.Properties where

import Flowbox.Prelude
import Luna.DEP.Graph.Attributes               (Attributes)
import Luna.DEP.Graph.Flags                    (Flags)
import Luna.DEP.Graph.View.Default.DefaultsMap (DefaultsMap)



data Properties = Properties { _flags       :: Flags
                             , _defaultsMap :: DefaultsMap
                             , _attrs       :: Attributes
                             } deriving (Show, Eq, Read)

makeLenses ''Properties


instance Default Properties where
    def = Properties def def def
