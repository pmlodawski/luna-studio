---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Graph.View.EdgeView where

import           Luna.Data.Graph.Edge                (Edge)
import qualified Luna.Data.Graph.Edge                as Edge
import qualified Luna.Data.Graph.Port                as Port
import           Luna.Data.Graph.View.PortDescriptor (PortDescriptor)
import           Flowbox.Prelude



data EdgeView = EdgeView { _src :: PortDescriptor
                         , _dst :: PortDescriptor
                         } deriving (Show, Read, Ord, Eq)


makeLenses(''EdgeView)

fromEdge :: Edge -> Maybe EdgeView
fromEdge (Edge.Monadic            ) = Nothing
fromEdge (Edge.Data (Port.Num s) d) = Just $ EdgeView [s] [d]
fromEdge (Edge.Data  Port.All    d) = Just $ EdgeView []  [d]

