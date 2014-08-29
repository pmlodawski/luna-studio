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

module Luna.Interpreter.Proto.CallPointPath where

import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.CallPointPath      as Gen
import           Luna.Interpreter.Proto.CallPoint               ()
import           Luna.Interpreter.Session.Data.CallPointPath    (CallPointPath)



instance Convert CallPointPath Gen.CallPointPath where
    encode = Gen.CallPointPath . encodeList
    decode (Gen.CallPointPath tpath) = decodeList tpath
