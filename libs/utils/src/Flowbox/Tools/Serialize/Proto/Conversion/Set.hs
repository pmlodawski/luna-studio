---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Tools.Serialize.Proto.Conversion.Set where

import           Data.IntSet   (IntSet)
import qualified Data.IntSet   as IntSet
import           Data.Sequence (Seq)

import Flowbox.Prelude
import Flowbox.Tools.Conversion.Proto
import Flowbox.Tools.Serialize.Proto.Conversion.List ()



instance ConvertPure Int b => ConvertPure IntSet (Seq b) where
    encodeP = encodeP . IntSet.toList
    decodeP = IntSet.fromList . decodeP
