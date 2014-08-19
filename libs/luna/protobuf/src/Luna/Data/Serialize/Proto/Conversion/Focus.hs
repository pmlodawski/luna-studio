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

module Luna.Data.Serialize.Proto.Conversion.Focus where

import           Control.Applicative
import qualified Data.Map                        as Map
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Zipper.Focus                   (Focus)
import qualified Flowbox.Luna.Data.AST.Zipper.Focus                   as Focus
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr   ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Module ()
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Focus.ClassFocus                     as GenClass
import qualified Generated.Proto.Focus.Focus                          as Gen
import qualified Generated.Proto.Focus.Focus.Cls                      as GenCls
import qualified Generated.Proto.Focus.FunctionFocus                  as GenFunction
import qualified Generated.Proto.Focus.ModuleFocus                    as GenModule



instance Convert Focus Gen.Focus where
    encode t = case t of
        Focus.Function f -> genFocus GenCls.FunctionFocus GenFunction.ext $ GenFunction.FunctionFocus $ encodeJ f
        Focus.Class    c -> genFocus GenCls.ClassFocus    GenClass.ext    $ GenClass.ClassFocus       $ encodeJ c
        Focus.Module   m -> genFocus GenCls.ModuleFocus   GenModule.ext   $ GenModule.ModuleFocus     $ encodeJ m
        where
            genFocus :: GenCls.Cls -> Extensions.Key Maybe Gen.Focus v -> v -> Gen.Focus
            genFocus cls key ext = Extensions.putExt key (Just ext)
                                $ Gen.Focus cls $ Extensions.ExtField Map.empty

    decode t@(Gen.Focus cls _) = case cls of
        GenCls.FunctionFocus -> do
            ext <- getExt GenFunction.ext
            (GenFunction.FunctionFocus mtf) <- ext <?> "Failed to decode Focus.FunctionFocus: extension is missing"
            tf  <- mtf <?> "Failed to decode Focus.FunctionFocus: 'f' field is missing"
            Focus.Function <$> decode tf
        GenCls.ClassFocus -> do
            ext <- getExt GenClass.ext
            (GenClass.ClassFocus mtf) <- ext <?> "Failed to decode Focus.ClassFocus: extension is missing"
            tf  <- mtf <?> "Failed to decode Focus.ClassFocus: 'c' field is missing"
            Focus.Class <$> decode tf
        GenCls.ModuleFocus -> do
            ext <- getExt GenModule.ext
            (GenModule.ModuleFocus mtf) <- ext <?> "Failed to decode Focus.ModuleFocus: extension is missing"
            tf  <- mtf <?> "Failed to decode Focus.ModuleFocus: 'm' field is missing"
            Focus.Module <$> decode tf
       where getExt = flip Extensions.getExt t
