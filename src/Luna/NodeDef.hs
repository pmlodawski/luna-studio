---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.NodeDef(
NodeDef(..),
noPorts
) where

import Luna.Common(NodeDef(..))

noPorts :: [String]
noPorts = []
