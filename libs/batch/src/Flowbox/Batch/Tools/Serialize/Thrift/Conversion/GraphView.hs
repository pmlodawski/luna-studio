---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.GraphView where

import qualified Data.HashMap.Strict                                       as HashMap
import           Data.HashMap.Strict                                         (HashMap)
import           Data.Text.Lazy                                              (Text, pack, unpack)
import qualified Data.Vector                                               as Vector
import           Data.Vector                                                 (Vector)

import qualified Graphview_Types                                           as TGraphView
import qualified Flowbox.Batch.GraphView.EdgeView                          as EdgeView
import           Flowbox.Batch.GraphView.EdgeView                            (EdgeView(..))
import qualified Flowbox.Batch.GraphView.GraphView                         as GraphView
import           Flowbox.Batch.GraphView.GraphView                           (GraphView)
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Conversion
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs         ()
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Graph



instance Convert GraphView TGraphView.GraphView where
    encode agraph = TGraphView.GraphView n e where 
        (n, e) = encodeGraph agraph
    decode (TGraphView.GraphView mtnodes mtedges) = decodeGraph (mtnodes, mtedges)




instance Convert (Int, Int, EdgeView) TGraphView.EdgeView where
  encode (srcNode, dstNode, EdgeView srcPort dstPort) = let 
      tsrcNode = itoi32 srcNode
      tdstNode = itoi32 dstNode
      tsrcPort = Vector.fromList $ map (itoi32) srcPort
      tdstPort = itoi32 dstPort
    in TGraphView.EdgeView (Just tsrcNode) (Just tdstNode) (Just tsrcPort) (Just tdstPort)
  decode (TGraphView.EdgeView mtsrcNode mtdstNode mtsrcPort mtdstPort) = case mtsrcNode of
    Nothing                   -> Left "`srcNode` field is missing"
    Just tsrcNode             -> case mtdstNode of
        Nothing               -> Left "`dstNode` field is missing"
        Just tdstNode         -> case mtsrcPort of
            Nothing           -> Left "`srcPort` field is missing"
            Just tsrcPort     -> case mtdstPort of
                Nothing       -> Left "`srcPort` field is missing"
                Just tdstPort -> Right (srcNode, dstNode, EdgeView srcPort dstPort) where
                    srcNode = i32toi tsrcNode
                    dstNode = i32toi tdstNode
                    srcPort = map (i32toi) $ Vector.toList tsrcPort
                    dstPort = i32toi tdstPort
