---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.DEP.Data.Serialize.Proto.Conversion.GraphView where

import           Control.Applicative

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Graphview.EdgeView         as Gen
import qualified Generated.Proto.Dep.Graphview.GraphView        as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.Graph ()
import           Luna.DEP.Graph.View.EdgeView                   (EdgeView (EdgeView))
import           Luna.DEP.Graph.View.GraphView                  (GraphView)
import qualified Luna.DEP.Graph.View.GraphView                  as GraphView



instance Convert (Int, Int, EdgeView) Gen.EdgeView where
    encode (nodeSrc, nodeDst, EdgeView portSrc portDst) =
        Gen.EdgeView (encodePJ nodeSrc) (encodePJ nodeDst) (encodeP portSrc) (encodeP portDst)
    decode (Gen.EdgeView mtnodeSrc mtnodeDst tportSrc tportDst) = do
        tnodeSrc <- mtnodeSrc <?> "Failed to decode EdgeView: 'srcNode' field is missing"
        tnodeDst <- mtnodeDst <?> "Failed to decode EdgeView: 'dstNode' field is missing"
        return (decodeP tnodeSrc, decodeP tnodeDst, EdgeView (decodeP tportSrc) (decodeP tportDst))


instance Convert GraphView Gen.GraphView where
    encode graph =
        Gen.GraphView (encode $ GraphView.labNodes graph) (encode $ GraphView.labEdges graph)
    decode (Gen.GraphView tnodes tedges) =
        GraphView.mkGraph <$> decode tnodes <*> decode tedges
