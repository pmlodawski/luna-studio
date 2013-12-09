---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flowbox.Tools.Serialize.Proto.Conversion.String where

import qualified Text.ProtocolBuffers.Basic as Proto

import Flowbox.Prelude
import Flowbox.Tools.Conversion.Proto


instance ConvertPure String Proto.Utf8 where
    encodeP = Proto.uFromString
    decodeP = Proto.uToString

