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

import           Flowbox.Prelude
import           Flowbox.Control.Error
import           Flowbox.Luna.Data.Graph.Edge                             (Edge (Edge))
import qualified Flowbox.Luna.Data.Graph.Graph                            as Graph
import           Flowbox.Luna.Data.Graph.Graph                            (Graph)
import qualified Flowbox.Luna.Data.Graph.Node                             as Node
import           Flowbox.Luna.Data.Graph.Node                             (Node)
import qualified Flowbox.Luna.Data.Graph.Port                             as Port
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Attributes ()
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
        Node.Expr expr _ flags attrs -> Gen.Node GenNode.Expr    (encodePJ nodeID) (encodePJ expr) (encodeJ flags) (encodePJ attrs)
        Node.Inputs      flags attrs -> Gen.Node GenNode.Inputs  (encodePJ nodeID) Nothing         (encodeJ flags) (encodePJ attrs)
        Node.Outputs     flags attrs -> Gen.Node GenNode.Outputs (encodePJ nodeID) Nothing         (encodeJ flags) (encodePJ attrs)
    decode (Gen.Node tcls mtnodeID mtexpr mtflags mtattrs) = do
        nodeID <- decodeP <$> mtnodeID <?> "Failed to decode Node: 'nodeID' field is missing"
        attrs  <- decodeP <$> mtattrs  <?> "Failed to decode Node: 'attrs' field is missing"
        tflags <- mtflags <?> "Failed to decode Node: 'flags' field is missing"
        flags  <- decode tflags
        node <- case tcls of
            GenNode.Expr -> do expr <- decodeP <$> mtexpr <?> "Failed to decode Node: 'expr' field is missing"
                               return $ Node.Expr expr Nothing
            GenNode.Inputs  -> return Node.Inputs
            GenNode.Outputs -> return Node.Outputs
        return (nodeID, node flags attrs)


instance Convert Graph Gen.Graph where
    encode graph =
        Gen.Graph (encodeList $ Graph.labNodes graph) (encodeList $ Graph.labEdges graph)
    decode (Gen.Graph tnodes tedges) =
        Graph.mkGraph <$> decodeList tnodes <*> decodeList tedges
