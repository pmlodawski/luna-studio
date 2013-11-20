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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Lit where

import           Flowbox.Prelude                                  
import           Flowbox.Control.Error                            
import qualified Flowbox.Luna.Data.AST.Lit                      as Lit
import           Flowbox.Luna.Data.AST.Lit                        (Lit)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic   
import qualified Generated.Proto.Lit.Lit                        as Gen
import qualified Generated.Proto.Lit.Lit.Cls                    as Gen



instance Convert Lit Gen.Lit where
    encode lit = case lit of 
        Lit.Char    i char -> Gen.Lit Gen.Char    (Just $ encodeP i) (Just $ encodeP [char])
        Lit.String  i str  -> Gen.Lit Gen.String  (Just $ encodeP i) (Just $ encodeP str)
        Lit.Integer i str  -> Gen.Lit Gen.Integer (Just $ encodeP i) (Just $ encodeP str)
        Lit.Float   i str  -> Gen.Lit Gen.Float   (Just $ encodeP i) (Just $ encodeP str)
    decode (Gen.Lit dtype mtid mtstr) = do 
        tid  <- mtid  <?> "Failed to decode Lit: 'id' field is missing"
        tstr <- mtstr <?> "Failed to decode Lit: 'str' field is missing"
        return $ case dtype of
            Gen.Char    -> Lit.Char    (decodeP tid) (head $ decodeP tstr)
            Gen.String  -> Lit.String  (decodeP tid) (decodeP tstr)
            Gen.Integer -> Lit.Integer (decodeP tid) (decodeP tstr)
            Gen.Float   -> Lit.Float   (decodeP tid) (decodeP tstr)
