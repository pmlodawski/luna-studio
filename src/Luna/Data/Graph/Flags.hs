---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Graph.Flags where

import Flowbox.Prelude



data Flags = Flags { _io   :: Bool
                   , _omit :: Bool
                   } deriving (Show, Read, Eq)


makeLenses (''Flags)


instance Default Flags where
    def = Flags False False
