---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Data.Mode (
    module X,
) where

import Flowbox.Prelude
import Generated.Proto.Mode.Mode as X


instance Default Mode where
    def = Mode Nothing
