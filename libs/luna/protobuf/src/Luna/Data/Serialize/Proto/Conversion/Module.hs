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

import           Control.Applicative

import           Flowbox.Data.Convert
import qualified Generated.Proto.Module.Module             as Gen
import           Luna.Data.Serialize.Proto.Conversion.Expr ()
import           Luna.Data.Serialize.Proto.Conversion.Type ()
import           Luna.DEP.AST.Module                       (Module (Module))


instance Convert Module Gen.Module where
    encode (Module i cls imports classes typeAliases typeDefs fields methods modules) =
        Gen.Module (encodePJ i) (encodeJ cls) (encode imports)
                   (encode classes)  (encode typeAliases)
                   (encode typeDefs) (encode fields)
                   (encode methods)  (encode modules)
    decode (Gen.Module i cls imports classes typeAliases typeDefs fields methods modules) =
        Module <$> decodePJ i   (missing "Module" "id" )
               <*> decodeJ  cls (missing "Module" "cls")
               <*> decode imports  <*> decode classes <*> decode typeAliases
               <*> decode typeDefs <*> decode fields  <*> decode methods
               <*> decode modules
