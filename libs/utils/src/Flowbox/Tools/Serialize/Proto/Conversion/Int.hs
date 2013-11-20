---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flowbox.Tools.Serialize.Proto.Conversion.Int where

import           Data.Int                          (Int32)

import           Flowbox.Prelude                   
import           Flowbox.Tools.Conversion.Proto    
import qualified Flowbox.Tools.Conversion.Common as Common



instance ConvertPure Int Int32 where
    encodeP = Common.itoi32
    decodeP = Common.i32toi

