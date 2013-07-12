---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Node(
Node(..)
) where

import Data.Word(Word8)
import Luna.Common(Node(..))
import Data.GraphViz.Attributes (Labellable, toLabelValue)

instance Labellable Node where
	toLabelValue = toLabelValue . show
