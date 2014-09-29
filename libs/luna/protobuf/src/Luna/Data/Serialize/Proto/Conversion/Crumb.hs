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

module Luna.Data.Serialize.Proto.Conversion.Crumb where

import           Control.Applicative
import qualified Data.Sequence                                  as Seq
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Crumb.ASTPtr                   as Gen
import qualified Generated.Proto.Crumb.Breadcrumbs              as Gen
import qualified Generated.Proto.Crumb.Crumb                    as Gen
import qualified Generated.Proto.Crumb.Crumb.Cls                as GenCls
import           Luna.AST.Control.Crumb                         (Breadcrumbs)
import           Luna.AST.Control.Crumb                         (Crumb)
import qualified Luna.AST.Control.Crumb                         as Crumb
import qualified Luna.Lib.Lib                                   as Lib



instance Convert Crumb Gen.Crumb where
    encode crumb = case crumb of
        Crumb.Function name path -> Gen.Crumb GenCls.FunctionCrumb (encodePJ name) (encodeListP path)
        Crumb.Class    name      -> Gen.Crumb GenCls.ClassCrumb    (encodePJ name) Seq.empty
        Crumb.Module   name      -> Gen.Crumb GenCls.ModuleCrumb   (encodePJ name) Seq.empty
    decode (Gen.Crumb tcls mtname tpath) = do
        name <- decodeP <$> mtname <?> "Failed to decode Crumb: 'name' field is missing"
        pure $ case tcls of
            GenCls.FunctionCrumb -> Crumb.Function name (decodeListP tpath)
            GenCls.ClassCrumb    -> Crumb.Class    name
            GenCls.ModuleCrumb   -> Crumb.Module   name


instance Convert Breadcrumbs Gen.Breadcrumbs where
    encode = Gen.Breadcrumbs . encodeList
    decode (Gen.Breadcrumbs b) = decodeList b


instance Convert (Breadcrumbs, Lib.ID) Gen.ASTPtr where
    encode (bc, libraryID) = Gen.ASTPtr (encodeJ bc) (encodePJ libraryID)
    decode (Gen.ASTPtr mtbc mtlibraryID) = do
        tbc <- mtbc <?> "Failed to decode ASTPtr: 'breadcrumbs' field is missing"
        bc  <- decode tbc
        libraryID <- decodeP <$> mtlibraryID <?> "Failed to decode ASTPtr: 'libraryID' field is missing"
        return (bc, libraryID)
