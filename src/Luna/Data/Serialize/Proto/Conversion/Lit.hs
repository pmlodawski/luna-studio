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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Lit where

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Lit                      (Lit)
import qualified Flowbox.Luna.Data.AST.Lit                      as Lit
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Lit.Lit                        as Gen
import qualified Generated.Proto.Lit.Lit.Cls                    as Gen



instance Convert Lit Gen.Lit where
    encode lit = case lit of
        Lit.Char    i char -> Gen.Lit Gen.Char    (encodePJ i) (encodePJ [char])
        Lit.String  i str  -> Gen.Lit Gen.String  (encodePJ i) (encodePJ str)
        Lit.Integer i str  -> Gen.Lit Gen.Integer (encodePJ i) (encodePJ str)
        Lit.Float   i str  -> Gen.Lit Gen.Float   (encodePJ i) (encodePJ str)
    decode (Gen.Lit dtype mtid mtstr) = do
        tid  <- mtid  <?> "Failed to decode Lit: 'id' field is missing"
        tstr <- mtstr <?> "Failed to decode Lit: 'str' field is missing"
        return $ case dtype of
            Gen.Char    -> Lit.Char    (decodeP tid) (head $ decodeP tstr)
            Gen.String  -> Lit.String  (decodeP tid) (decodeP tstr)
            Gen.Integer -> Lit.Integer (decodeP tid) (decodeP tstr)
            Gen.Float   -> Lit.Float   (decodeP tid) (decodeP tstr)
