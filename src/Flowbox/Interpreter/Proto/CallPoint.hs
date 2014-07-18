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

module Flowbox.Interpreter.Proto.CallPoint where

import           Flowbox.Control.Error
import           Flowbox.Interpreter.Session.Data.CallPoint     (CallPoint (CallPoint))
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.CallPoint          as Gen



instance Convert CallPoint Gen.CallPoint where
    encode (CallPoint libraryID nodeID) = Gen.CallPoint tlibraryID tnodeID where
        tlibraryID = encodePJ libraryID
        tnodeID    = encodePJ nodeID
    decode (Gen.CallPoint mtlibraryID mtnodeID) = do
        libraryID <- decodeP <$> mtlibraryID <?> "Failed to decode CallPoint: 'libraryID' field is missing"
        nodeID    <- decodeP <$> mtnodeID    <?> "Failed to decode CallPoint: 'nodeID' field is missing"
        return $ CallPoint libraryID nodeID
