---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.EdgeView(
	EdgeView(..),
) where

import           Flowbox.Prelude                           
import           Flowbox.Batch.GraphView.PortDescriptor   (PortDescriptor)



data EdgeView = EdgeView { srcPort :: PortDescriptor
                         , dstPort :: PortDescriptor
                         } deriving (Show, Read, Ord, Eq)




