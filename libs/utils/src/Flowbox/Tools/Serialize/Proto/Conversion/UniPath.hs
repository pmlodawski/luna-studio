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
module Flowbox.Tools.Serialize.Proto.Conversion.UniPath where

import qualified Text.ProtocolBuffers.Basic as Proto

import           Flowbox.Prelude
import           Flowbox.System.UniPath                          (UniPath)
import qualified Flowbox.System.UniPath                          as UniPath
import           Flowbox.Tools.Conversion.Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.String ()



instance ConvertPure UniPath Proto.Utf8 where
    encodeP = encodeP . UniPath.toUnixString
    decodeP = UniPath.fromUnixString . decodeP

