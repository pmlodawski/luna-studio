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

module Luna.Interpreter.Proto.CallPoint where

import           Flowbox.Data.Convert
import qualified Generated.Proto.Interpreter.CallPoint            as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.Library ()
import           Luna.Interpreter.Session.Data.CallPoint          (CallPoint (CallPoint))



instance ConvertPure CallPoint Gen.CallPoint where
    encodeP (CallPoint     libraryID nodeID) = Gen.CallPoint (encodeP libraryID) (encodeP nodeID)
    decodeP (Gen.CallPoint libraryID nodeID) = CallPoint     (decodeP libraryID) (decodeP nodeID)
