---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Data.GraphView.EdgeView where

import           Flowbox.Luna.Data.Graph.Edge               (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Port               as Port
import           Flowbox.Luna.Data.GraphView.PortDescriptor (PortDescriptor)
import           Flowbox.Prelude



data EdgeView = EdgeView { _src :: PortDescriptor
                         , _dst :: PortDescriptor
                         } deriving (Show, Read, Ord, Eq)


makeLenses(''EdgeView)

fromEdge :: Edge -> EdgeView
fromEdge (Edge (Port.Num s) d) = EdgeView [s] [d]
fromEdge (Edge  Port.All    d) = EdgeView []  [d]


toEdge :: EdgeView -> Either String Edge
toEdge (EdgeView src' dst') = do s <- case src' of
                                        [s] -> return $ Port.Num s
                                        []  -> return Port.All
                                        _   -> Left "Source ports with size greater than 1 are not supported"
                                 d <- case dst' of
                                        [d] -> return d
                                        _   -> Left "Destination ports with size other than 1 are not supported"
                                 return $ Edge s d


toLEdge :: (s, d, EdgeView) -> Either String (s, d, Edge)
toLEdge (s, d, ev) = do e <- toEdge ev
                        return (s, d, e)
