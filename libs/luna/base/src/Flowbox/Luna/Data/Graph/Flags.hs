---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.Graph.Flags where

import Flowbox.Prelude



data Flags = Flags { _io   :: Bool
                   , _omit :: Bool
                   } deriving (Show, Read, Eq)


makeLenses (''Flags)


empty :: Flags
empty = Flags False False
