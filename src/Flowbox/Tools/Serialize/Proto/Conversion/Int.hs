---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flowbox.Tools.Serialize.Proto.Conversion.Int where

import Data.Int (Int32)

import           Flowbox.Prelude
import qualified Flowbox.Tools.Conversion.Common as Common
import           Flowbox.Tools.Conversion.Proto



instance ConvertPure Int Int32 where
    encodeP = Common.itoi32
    decodeP = Common.i32toi

