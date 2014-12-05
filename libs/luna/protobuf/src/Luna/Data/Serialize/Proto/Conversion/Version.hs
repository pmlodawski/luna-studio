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

module Luna.Data.Serialize.Proto.Conversion.Version where

import           Data.Version                                   (Version (Version))
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Version.Version                as Gen



instance ConvertPure Version Gen.Version where
    encodeP (Version branch tags)     = Gen.Version (encodeListP branch) (encodeListP tags)
    decodeP (Gen.Version branch tags) = Version     (decodeListP branch) (decodeListP tags)
