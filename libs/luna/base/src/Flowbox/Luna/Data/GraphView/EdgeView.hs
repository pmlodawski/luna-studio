---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.GraphView.EdgeView where

import           Flowbox.Luna.Data.Graph.Edge               (Edge)
import qualified Flowbox.Luna.Data.Graph.Edge               as Edge
import qualified Flowbox.Luna.Data.Graph.Port               as Port
import           Flowbox.Luna.Data.GraphView.PortDescriptor (PortDescriptor)
import           Flowbox.Prelude



data EdgeView = EdgeView { _src :: PortDescriptor
                         , _dst :: PortDescriptor
                         } deriving (Show, Read, Ord, Eq)


makeLenses(''EdgeView)

fromEdge :: Edge -> Maybe EdgeView
fromEdge (Edge.Monadic            ) = Nothing
fromEdge (Edge.Data (Port.Num s) d) = Just $ EdgeView [s] [d]
fromEdge (Edge.Data  Port.All    d) = Just $ EdgeView []  [d]

