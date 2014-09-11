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

module Luna.Interpreter.Proto.Status where

import qualified Flowbox.Batch.Project.Project                  as Project
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.Status             as Gen
import           Luna.Data.Serialize.Proto.Conversion.Crumb     ()
import           Luna.Interpreter.Session.Cache.Value           (Status)
import qualified Luna.Interpreter.Session.Cache.Value           as Value



instance ConvertPure Status Gen.Status where
    encodeP Value.Ready        = Gen.Ready
    encodeP Value.Modified     = Gen.Modified
    encodeP Value.NonCacheable = Gen.NonCacheable
    encodeP Value.NotInCache   = Gen.NotInCache
    encodeP Value.Unknown      = Gen.Unknown

    decodeP Gen.Ready        = Value.Ready
    decodeP Gen.Modified     = Value.Modified
    decodeP Gen.NonCacheable = Value.NonCacheable
    decodeP Gen.NotInCache   = Value.NotInCache
    decodeP Gen.Unknown      = Value.Unknown
