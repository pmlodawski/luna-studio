---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.GraphView where

import Control.Applicative

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.GraphView.EdgeView                (EdgeView (EdgeView))
import           Flowbox.Luna.Data.GraphView.GraphView               (GraphView)
import qualified Flowbox.Luna.Data.GraphView.GraphView               as GraphView
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Graph ()
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Graphview.EdgeView                  as Gen
import qualified Generated.Proto.Graphview.GraphView                 as Gen



instance Convert (Int, Int, EdgeView) Gen.EdgeView where
    encode (nodeSrc, nodeDst, EdgeView portSrc portDst) =
        Gen.EdgeView (encodePJ nodeSrc) (encodePJ nodeDst) (encodeListP portSrc) (encodeListP portDst)
    decode (Gen.EdgeView mtnodeSrc mtnodeDst tportSrc tportDst) = do
        tnodeSrc <- mtnodeSrc <?> "Failed to decode EdgeView: 'srcNode' field is missing"
        tnodeDst <- mtnodeDst <?> "Failed to decode EdgeView: 'dstNode' field is missing"
        return $ (decodeP tnodeSrc, decodeP tnodeDst, EdgeView (decodeListP tportSrc) (decodeListP tportDst))


instance Convert GraphView Gen.GraphView where
    encode graph =
        Gen.GraphView (encodeList $ GraphView.labNodes graph) (encodeList $ GraphView.labEdges graph)
    decode (Gen.GraphView tnodes tedges) =
        GraphView.mkGraph <$> decodeList tnodes <*> decodeList tedges
