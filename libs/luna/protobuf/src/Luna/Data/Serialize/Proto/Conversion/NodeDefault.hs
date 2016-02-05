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

module Luna.Data.Serialize.Proto.Conversion.NodeDefault where

import qualified Data.Map                                      as Map

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Nodedefault.DefaultsMap       as Gen
import qualified Generated.Proto.Nodedefault.DefaultsMap.Entry as Gen
import           Luna.Data.Serialize.Proto.Conversion.Graph    ()
import qualified Luna.Syntax.Graph.Node                        as Node
import           Luna.Syntax.Graph.Node.Expr                   (NodeExpr)
import           Luna.Syntax.Graph.View.Default.DefaultsMap    (DefaultsMap)
import           Luna.Syntax.Graph.View.PortDescriptor         (PortDescriptor)



instance Convert DefaultsMap Gen.DefaultsMap where
    encode = Gen.DefaultsMap . encode . Map.toList
    decode (Gen.DefaultsMap items) = Map.fromList <$> decode items


instance Convert (PortDescriptor, (Node.ID, NodeExpr)) Gen.Entry where
    encode (inPort, (nodeID, value)) = Gen.Entry (encodeP inPort) (encodePJ nodeID) (encodeJ value)
    decode (Gen.Entry inPort mnodeID mvalue) = do
        nodeID <- decodeP <$> mnodeID <?> "Failed to decode DefaultsMap.Entry: 'nodeID' field is missing"
        value  <- decode  =<< mvalue  <?> "Failed to decode DefaultsMap.Entry: 'value' field is missing"
        return (decodeP inPort, (nodeID, value))
