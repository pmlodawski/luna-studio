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

module Luna.DEP.Data.Serialize.Proto.Conversion.NodeDefault where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Nodedefault.DefaultsMap       as Gen
import qualified Generated.Proto.Dep.Nodedefault.DefaultsMap.Entry as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.Graph    ()
import qualified Luna.DEP.Graph.Node                               as Node
import           Luna.DEP.Graph.Node.Expr                          (NodeExpr)
import           Luna.DEP.Graph.View.Default.DefaultsMap           (DefaultsMap)
import           Luna.DEP.Graph.View.Default.Expr                  (DefaultExpr (DefaultExpr))
import           Luna.DEP.Graph.View.PortDescriptor                (PortDescriptor)



instance Convert DefaultsMap Gen.DefaultsMap where
    encode = Gen.DefaultsMap . encode . Map.toList
    decode (Gen.DefaultsMap items) = Map.fromList <$> decode items


instance Convert (PortDescriptor, DefaultExpr) Gen.Entry where
    encode (inPort, DefaultExpr nodeID originID value) = Gen.Entry (encodeP inPort) (encodePJ nodeID) (encodePJ originID) (encodeJ value)
    decode (Gen.Entry inPort mnodeID moriginID mvalue) = do
        tnodeID  <- mnodeID   <?> "Failed to decode DefaultsMap.Entry: 'nodeID' field is missing"
        let nodeID = decodeP tnodeID
        value    <- decode  =<< mvalue    <?> "Failed to decode DefaultsMap.Entry: 'value' field is missing"
        let originID = decodeP $ Maybe.fromMaybe tnodeID moriginID
        return (decodeP inPort, DefaultExpr nodeID originID value)
