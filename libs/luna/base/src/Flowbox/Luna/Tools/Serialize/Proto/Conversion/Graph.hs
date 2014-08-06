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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Graph where

import Control.Applicative
import Data.Int            (Int32)

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.Graph.Edge                   (Edge)
import qualified Flowbox.Luna.Data.Graph.Edge                   as Edge
import           Flowbox.Luna.Data.Graph.Graph                  (Graph)
import qualified Flowbox.Luna.Data.Graph.Graph                  as Graph
import           Flowbox.Luna.Data.Graph.Node                   (Node)
import qualified Flowbox.Luna.Data.Graph.Node                   as Node
import           Flowbox.Luna.Data.Graph.Port                   (OutPort)
import qualified Flowbox.Luna.Data.Graph.Port                   as Port
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Graph.Edge                     as Gen
import qualified Generated.Proto.Graph.Graph                    as Gen
import qualified Generated.Proto.Graph.Node                     as Gen
import qualified Generated.Proto.Graph.Node.Cls                 as GenNode



instance Convert (Int, Int, Edge) Gen.Edge where
    encode (nodeSrc, nodeDst, Edge.Data portSrc portDst) =
        Gen.Edge (encodePJ nodeSrc) (encodePJ nodeDst) (encodeP portSrc) (encodePJ portDst)
    decode (Gen.Edge mtnodeSrc mtnodeDst mtportSrc mtportDst) = do
        tnodeSrc <- mtnodeSrc <?> "Failed to decode Edge: 'srcNode' field is missing"
        tnodeDst <- mtnodeDst <?> "Failed to decode Edge: 'dstNode' field is missing"
        tportDst <- mtportDst <?> "Failed to decode Edge: 'dstPort' field is missing"
        return (decodeP tnodeSrc, decodeP tnodeDst, Edge.Data (decodeP mtportSrc) (decodeP tportDst))


instance Convert (Int, Node) Gen.Node where
    encode (nodeID, node) = tnode where
        tnodeID = encodePJ nodeID
        tnodeWithoutPos = case node of
            Node.Expr    {} -> Gen.Node GenNode.Expr    tnodeID (encodePJ $ node ^. Node.expr)
                                                                (encodePJ $ node ^. Node.outputName)
            Node.Inputs  {} -> Gen.Node GenNode.Inputs  tnodeID Nothing Nothing
            Node.Outputs {} -> Gen.Node GenNode.Outputs tnodeID Nothing Nothing
        tnode = tnodeWithoutPos (Just $ node ^. Node.pos . _1) (Just $ node ^. Node.pos . _2)
    decode (Gen.Node tcls mtnodeID mtexpr mtoutputName mx my) = do
        nodeID <- decodeP <$> mtnodeID <?> "Failed to decode Node: 'id' field is missing"
        x <- mx <?> "Failed to decode Node: 'x' field is missing"
        y <- my <?> "Failed to decode Node: 'y' field is missing"
        node <- case tcls of
            GenNode.Expr -> do expr       <- decodeP <$> mtexpr       <?> "Failed to decode Node: 'expr' field is missing"
                               outputName <- decodeP <$> mtoutputName <?> "Failed to decode Node: 'outputName' field is missing"
                               return $ Node.Expr expr outputName
            GenNode.Inputs  -> return Node.Inputs
            GenNode.Outputs -> return Node.Outputs
        return (nodeID, node (x, y ))


instance Convert Graph Gen.Graph where
    encode graph =
        Gen.Graph (encodeList $ Graph.labNodes graph) (encodeList $ Graph.labEdges graph)
    decode (Gen.Graph tnodes tedges) =
        Graph.mkGraph <$> decodeList tnodes <*> decodeList tedges


instance ConvertPure OutPort (Maybe Int32) where
    encodeP port = case port of
        Port.All   -> Nothing
        Port.Num n -> encodePJ n
    decodeP mtport = case mtport of
        Nothing -> Port.All
        Just tn -> Port.Num $ decodeP tn
