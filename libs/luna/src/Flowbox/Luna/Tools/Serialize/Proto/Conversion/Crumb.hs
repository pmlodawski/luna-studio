---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Crumb where

import Control.Applicative

import           Flowbox.Control.Error
import           Flowbox.Luna.Data.AST.Crumb.Breadcrumbs        (Breadcrumbs)
import           Flowbox.Luna.Data.AST.Crumb.Crumb              (Crumb)
import qualified Flowbox.Luna.Data.AST.Crumb.Crumb              as Crumb
import qualified Flowbox.Luna.Lib.Library                       as Library
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Crumb.ASTPtr                   as Gen
import qualified Generated.Proto.Crumb.Breadcrumbs              as Gen
import qualified Generated.Proto.Crumb.Crumb                    as Gen
import qualified Generated.Proto.Crumb.Crumb.Cls                as GenCls



instance Convert Crumb Gen.Crumb where
    encode crumb = case crumb of
        Crumb.FunctionCrumb name -> Gen.Crumb GenCls.FunctionCrumb $ encodePJ name
        Crumb.ClassCrumb    name -> Gen.Crumb GenCls.ClassCrumb    $ encodePJ name
        Crumb.ModuleCrumb   name -> Gen.Crumb GenCls.ModuleCrumb   $ encodePJ name
    decode (Gen.Crumb tcls mtname) = do
        name <- decodeP <$> mtname <?> "Failed to decode Crumb: 'name' field is missing"
        let cls = case tcls of
                    GenCls.FunctionCrumb -> Crumb.FunctionCrumb
                    GenCls.ClassCrumb    -> Crumb.ClassCrumb
                    GenCls.ModuleCrumb   -> Crumb.ModuleCrumb
        return $ cls name


instance Convert Breadcrumbs Gen.Breadcrumbs where
    encode = Gen.Breadcrumbs . encodeList
    decode (Gen.Breadcrumbs b) = decodeList b


instance Convert (Breadcrumbs, Library.ID) Gen.ASTPtr where
    encode (bc, libraryID) = Gen.ASTPtr (encodeJ bc) (encodePJ libraryID)
    decode (Gen.ASTPtr mtbc mtlibraryID) = do
        tbc <- mtbc <?> "Failed to decode ASTPtr: 'breadcrumbs' field is missing"
        bc  <- decode tbc
        libraryID <- decodeP <$> mtlibraryID <?> "Failed to decode ASTPtr: 'libraryID' field is missing"
        return (bc, libraryID)
