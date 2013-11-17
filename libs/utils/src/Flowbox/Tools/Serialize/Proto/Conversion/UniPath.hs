---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Flowbox.Tools.Serialize.Proto.Conversion.UniPath where

import qualified Text.ProtocolBuffers.Basic     as Proto

import           Flowbox.Prelude                  
import qualified Flowbox.System.UniPath         as UniPath
import           Flowbox.System.UniPath           (UniPath)
import           Flowbox.Tools.Conversion.Proto   


instance Convert UniPath Proto.Utf8 where
    encode = Proto.uFromString . UniPath.toUnixString
    decode = return . UniPath.fromUnixString . Proto.uToString

