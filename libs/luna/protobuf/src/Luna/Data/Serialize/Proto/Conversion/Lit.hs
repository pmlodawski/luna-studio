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

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import           Flowbox.Control.Error
import           Flowbox.Prelude                                hiding (exp)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Lit.Decimal                    as GenDecimal
import qualified Generated.Proto.Lit.Float                      as GenFloat
import qualified Generated.Proto.Lit.Lit                        as Gen
import qualified Generated.Proto.Lit.Lit.Cls                    as Gen
import qualified Generated.Proto.Lit.Number                     as GenNumber
import qualified Generated.Proto.Lit.Repr                       as Gen
import qualified Generated.Proto.Lit.Repr.Cls                   as GenCls
import qualified Generated.Proto.Lit.Sign                       as Gen
import           Luna.AST.Lit                                   (Lit)
import qualified Luna.AST.Lit                                   as Lit
import           Luna.AST.Lit.Number                            (Number (Number))
import qualified Luna.AST.Lit.Number                            as Number
import qualified Text.ProtocolBuffers.Extensions                as Extensions



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
        Number.Float   int frac -> genRepr GenCls.Float   GenFloat.ext   $ GenFloat.Float     (encodePJ int) (encodePJ frac)
        Number.Decimal int      -> genRepr GenCls.Decimal GenDecimal.ext $ GenDecimal.Decimal (encodePJ int)
        where genRepr cls key ext = Extensions.putExt key (Just ext)
                                    $ Gen.Repr cls $ Extensions.ExtField Map.empty
    decode t@(Gen.Repr cls _) = case cls of
        GenCls.Float   -> do
            GenFloat.Float int frac <- getExt GenFloat.ext "Repr.Float"
            Number.Float <$> decodePJ int  (missing "Repr.Float" "int")
                         <*> decodePJ frac (missing "Repr.Float" "frac")
        GenCls.Decimal -> do
            GenDecimal.Decimal int <- getExt GenDecimal.ext "Repr.Decimal"
            Number.Decimal <$> decodePJ int  (missing "Repr.Decimal" "int")
        where getExt key datatype = Extensions.getExt key t <?&> missing datatype "extension"


instance ConvertPure Number.Sign Gen.Sign where
    encodeP Number.Positive = Gen.Positive
    encodeP Number.Negative = Gen.Negative
    decodeP Gen.Positive = Number.Positive
    decodeP Gen.Negative = Number.Negative

