---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Data.Topic where

import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack, unpack)

import Flowbox.Prelude



type Topic = String


toByteString :: Topic -> ByteString
toByteString = pack


fromByteString :: ByteString -> Topic
fromByteString = unpack


error :: Topic
error = "error"


status :: Topic
status = "status"


update :: Topic
update = "update"
