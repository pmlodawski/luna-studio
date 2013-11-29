---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Library where

import           Control.Applicative                                  

import           Flowbox.Control.Error                                
import           Flowbox.Luna.Lib.Library                         (Library(Library))
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module   ()
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic       
import qualified Generated.Proto.Library.Library                      as Gen



instance Convert Library Gen.Library where
    encode (Library name path ast) = 
        Gen.Library (encodePJ name) (encodePJ path) (encodeJ ast)
    decode (Gen.Library mtname mtpath mtast) = do 
        name <- decodeP <$> mtname <?> "Failed to decode Library: 'name' field is missing"
        path <- decodeP <$> mtpath <?> "Failed to decode Library: 'path' field is missing"
        tast <- mtast   <?> "Failed to decode Library: 'ast' field is missing"
        Library name path <$> decode tast
