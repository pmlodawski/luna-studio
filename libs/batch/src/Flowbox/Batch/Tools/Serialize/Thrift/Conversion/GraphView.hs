---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView where

import qualified Data.Vector                                               as Vector

import qualified Graphview_Types                                           as TGraphView
import           Flowbox.Batch.GraphView.EdgeView                            (EdgeView(..))
import           Flowbox.Batch.GraphView.GraphView                           (GraphView)
import           Flowbox.Control.Error
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph        
import           Flowbox.Tools.Conversion

instance Convert GraphView TGraphView.GraphView where
    encode agraph = TGraphView.GraphView n e where 
        (n, e) = encodeGraph agraph
    decode (TGraphView.GraphView mtnodes mtedges) = decodeGraph (mtnodes, mtedges)


instance Convert (Int, Int, EdgeView) TGraphView.EdgeView where
    encode (asrcNode, adstNode, EdgeView asrcPort adstPort) = let 
        tsrcNode = itoi32 asrcNode
        tdstNode = itoi32 adstNode
        tsrcPort = Vector.fromList $ map (itoi32) asrcPort
        tdstPort = itoi32 adstPort
        in TGraphView.EdgeView (Just tsrcNode) (Just tdstNode) (Just tsrcPort) (Just tdstPort)
    decode (TGraphView.EdgeView mtsrcNode mtdstNode mtsrcPort mtdstPort) = do 
        tsrcNode <- mtsrcNode <?> "Failed to decode EdgeView: `srcNode` field is missing"
        tdstNode <- mtdstNode <?> "Failed to decode EdgeView: `dstNode` field is missing"
        tsrcPort <- mtsrcPort <?> "Failed to decode EdgeView: `srcPort` field is missing"
        tdstPort <- mtdstPort <?> "Failed to decode EdgeView: `dstPort` field is missing"
        let asrcNode = i32toi tsrcNode
            adstNode = i32toi tdstNode
            asrcPort = map (i32toi) $ Vector.toList tsrcPort
            adstPort = i32toi tdstPort
        return (asrcNode, adstNode, EdgeView asrcPort adstPort) 
                    
