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

module Luna.DEP.Data.Serialize.Proto.Conversion.Version where

import           Data.Version                        (Version (Version))
import           Flowbox.Data.Convert
import qualified Generated.Proto.Dep.Version.Version as Gen



instance ConvertPure Version Gen.Version where
    encodeP (Version branch tags)     = Gen.Version (encodeP branch) (encodeP tags)
    decodeP (Gen.Version branch tags) = Version     (decodeP branch) (decodeP tags)
