---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.GraphView.EdgeView(
	EdgeView(..),
	empty,
) where


data EdgeView = EdgeView { srcPort :: [Int]
                         , dstPort :: [Int]
                         } deriving (Show, Read, Ord, Eq)

empty :: [EdgeView]
empty = [] 



