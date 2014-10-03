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

module Luna.Data.Serialize.Proto.Conversion.Lit where

import qualified Data.Maybe as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Lit.Lit                        as Gen
import qualified Generated.Proto.Lit.Lit.Cls                    as Gen
import qualified Generated.Proto.Lit.Number                     as GenNumber
import qualified Generated.Proto.Lit.Repr                       as Gen
import qualified Generated.Proto.Lit.Sign                       as Gen
import qualified Generated.Proto.Lit.Repr.Cls                   as Gen
import           Luna.AST.Lit                                   (Lit)
import qualified Luna.AST.Lit                                   as Lit
import           Luna.AST.Lit.Number                            (Number (Number), Repr (Repr))
import qualified Luna.AST.Lit.Number                            as Number



instance Convert Lit Gen.Lit where
    encode lit = case lit of
        Lit.Char    i char -> Gen.Lit Gen.Char   (encodePJ i) (encodePJ [char]) Nothing
        Lit.String  i str  -> Gen.Lit Gen.String (encodePJ i) (encodePJ str)    Nothing
        Lit.Number  i num  -> Gen.Lit Gen.Number (encodePJ i) Nothing           (encodeJ num)
    decode (Gen.Lit dtype mtid str num) = do
        i  <- decodePJ mtid $ missing "Lit" "id"
        case dtype of
            Gen.Char   -> Lit.Char i . head <$> decodePJ str (missing "Lit" "str")
            Gen.String -> Lit.String i      <$> decodePJ str (missing "Lit" "str")
            Gen.Number -> Lit.Number i      <$> decodeJ  num (missing "Lit" "num")

instance Convert Number GenNumber.Number where
    encode (Number base repr exp sign) =
        GenNumber.Number (encodePJ base) (encodeJ repr) (fmap encode exp) (encodePJ sign)
    decode (GenNumber.Number base repr exp sign) =
        Number <$> decodePJ base (missing "Number" "base")
               <*> decodeJ  repr (missing "Number" "repr")
               <*> Maybe.maybe (pure Nothing) (fmap Just . decode) exp
               <*> decodePJ sign (missing "Number" "sign")

instance Convert Number.Repr Gen.Repr where
    encode repr = case repr of
        Number.Float   int frac -> Gen.Repr Gen.Float   (encodePJ int) (encodePJ frac)
        Number.Decimal int      -> Gen.Repr Gen.Decimal (encodePJ int) Nothing
    decode (Gen.Repr dtype int frac) = case dtype of
        Gen.Float   -> Number.Float   <$> decodePJ int  (missing "Repr" "int") 
                                      <*> decodePJ frac (missing "Repr" "frac") 
        Gen.Decimal -> Number.Decimal <$> decodePJ int  (missing "Repr" "int")

instance ConvertPure Number.Sign Gen.Sign where
    encodeP Number.Positive = Gen.Positive
    encodeP Number.Negative = Gen.Negative
    decodeP Gen.Positive = Number.Positive
    decodeP Gen.Negative = Number.Negative

