---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.PortDescriptor(
	PortDescriptor,
	empty,
) where


type PortDescriptor = [Int]

empty :: PortDescriptor
empty = []