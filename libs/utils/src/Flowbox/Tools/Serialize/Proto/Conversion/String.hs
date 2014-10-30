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
module Flowbox.Tools.Serialize.Proto.Conversion.String where

import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Text.ProtocolBuffers.Basic as Proto

import Flowbox.Prelude
import Flowbox.Tools.Conversion.Proto



instance ConvertPure String Proto.Utf8 where
    encodeP = Proto.uFromString
    decodeP = Proto.uToString

instance ConvertPure Text Proto.Utf8 where
    encodeP = encodeP . Text.unpack
    decodeP = Text.pack . decodeP
