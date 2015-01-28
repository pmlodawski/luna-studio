---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Properties where

import Flowbox.Prelude
import Luna.Syntax.Graph.Attributes               (Attributes)
import Luna.Syntax.Graph.Flags                    (Flags)
import Luna.Syntax.Graph.View.Default.DefaultsMap (DefaultsMap)



data Properties a v = Properties { _flags       :: Flags
                                 , _defaultsMap :: DefaultsMap a v
                                 , _attrs       :: Attributes
                                 } deriving (Show, Eq, Read)

makeLenses ''Properties


instance Default (Properties a v) where
    def = Properties def def def
