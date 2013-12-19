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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Graph where

import Control.Applicative

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.Graph.Edge                             (Edge (Edge))
import           Flowbox.Luna.Data.Graph.Graph                            (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                            as Graph
import           Flowbox.Luna.Data.Graph.Node                             (Node)
import qualified Flowbox.Luna.Data.Graph.Node                             as Node
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Graph.Edge                               as Gen
import qualified Generated.Proto.Graph.Graph                              as Gen
import qualified Generated.Proto.Graph.Node                               as Gen
import qualified Generated.Proto.Graph.Node.Cls                           as GenNode



instance Convert (Int, Int, Edge) Gen.Edge where
    encode (nodeSrc, nodeDst, Edge portSrc portDst) =
        Gen.Edge (encodePJ nodeSrc) (encodePJ nodeDst) tportSrc (encodePJ portDst) where
            tportSrc = portSrc >>= return . encodeP
    decode (Gen.Edge mtnodeSrc mtnodeDst mtportSrc mtportDst) = do
        tnodeSrc <- mtnodeSrc <?> "Failed to decode Edge: 'srcNode' field is missing"
        tnodeDst <- mtnodeDst <?> "Failed to decode Edge: 'dstNode' field is missing"
        tportDst <- mtportDst <?> "Failed to decode Edge: 'dstPort' field is missing"
        let mportSrc = mtportSrc >>= return . decodeP
        return $ (decodeP tnodeSrc, decodeP tnodeDst, Edge mportSrc (decodeP tportDst))


instance Convert (Int, Node) Gen.Node where
    encode (nodeID, node) = case node of
        Node.Expr expr _ outName properties 
                                -> Gen.Node GenNode.Expr    (encodePJ nodeID) (encodePJ expr) (encodePJ outName) (encodeJ properties)
        Node.Inputs  properties -> Gen.Node GenNode.Inputs  (encodePJ nodeID) Nothing Nothing (encodeJ properties)
        Node.Outputs properties -> Gen.Node GenNode.Outputs (encodePJ nodeID) Nothing Nothing (encodeJ properties)
    decode (Gen.Node tcls mtnodeID mtexpr mtoutputName mtproperties) = do
        nodeID <- decodeP <$> mtnodeID <?> "Failed to decode Node: 'id' field is missing"
        tproperties <- mtproperties    <?> "Failed to decode Node: 'properties' field is missing"
        properties  <- decode tproperties
        node <- case tcls of
            GenNode.Expr -> do expr       <- decodeP <$> mtexpr       <?> "Failed to decode Node: 'expr' field is missing"
                               outputName <- decodeP <$> mtoutputName <?> "Failed to decode Node: 'outputName' field is missing"
                               return $ Node.Expr expr Nothing outputName
            GenNode.Inputs  -> return Node.Inputs
            GenNode.Outputs -> return Node.Outputs
        return (nodeID, node properties)


instance Convert Graph Gen.Graph where
    encode graph =
        Gen.Graph (encodeList $ Graph.labNodes graph) (encodeList $ Graph.labEdges graph)
    decode (Gen.Graph tnodes tedges) =
        Graph.mkGraph <$> decodeList tnodes <*> decodeList tedges
