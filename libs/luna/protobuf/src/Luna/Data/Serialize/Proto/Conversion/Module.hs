---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Data.Serialize.Proto.Conversion.Module where

import Control.Applicative

import           Flowbox.Control.Error
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Module.Module                  as Gen
import           Luna.AST.Module                                (Module (Module))
import           Luna.Data.Serialize.Proto.Conversion.Expr      ()
import           Luna.Data.Serialize.Proto.Conversion.Type      ()


instance Convert Module Gen.Module where
    encode (Module i cls imports classes typeAliases typeDefs fields methods modules) =
        Gen.Module (encodePJ i) (encodeJ cls) (encodeList imports)
                   (encodeList classes)  (encodeList typeAliases)
                   (encodeList typeDefs) (encodeList fields)
                   (encodeList methods)  (encodeList modules)
    decode (Gen.Module mtid mtcls timports tclasses ttypeAliases ttypeDefs tfields tmethods tmodules) = do
        i    <- decodeP <$> mtid  <?> "Failed to decode Module: 'id' field is missing"
        tcls <- mtcls <?> "Failed to decode Module: 'cls' field is missing"
        Module i <$> decode tcls <*> decodeList timports <*> decodeList tclasses
                 <*> decodeList ttypeAliases <*> decodeList ttypeDefs
                 <*> decodeList tfields <*> decodeList tmethods <*> decodeList tmodules
