---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Data.Serialize.Proto.Conversion.Crumb where

import           Control.Applicative
import qualified Data.Map            as Map

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Crumb.ASTPtr                   as Gen
import qualified Generated.Proto.Crumb.Breadcrumbs              as Gen
import qualified Generated.Proto.Crumb.Class                    as GenClass
import qualified Generated.Proto.Crumb.Crumb                    as Gen
import qualified Generated.Proto.Crumb.Crumb.Cls                as GenCls
import qualified Generated.Proto.Crumb.Function                 as GenFunction
import qualified Generated.Proto.Crumb.Lambda                   as GenLambda
import qualified Generated.Proto.Crumb.Module                   as GenModule
import           Luna.AST.Control.Crumb                         (Breadcrumbs, Crumb)
import qualified Luna.AST.Control.Crumb                         as Crumb
import           Luna.Data.Serialize.Proto.Conversion.Name      ()
import qualified Luna.Lib.Lib                                   as Lib
import qualified Text.ProtocolBuffers.Extensions                as Extensions



instance Convert Crumb Gen.Crumb where
    encode crumb = case crumb of
        Crumb.Function name path -> genCrumb GenCls.Function GenFunction.ext $ GenFunction.Function
                                      (encodeJ name) (encodeListP path)
        Crumb.Class    name      -> genCrumb GenCls.Class  GenClass.ext  $ GenClass.Class   (encodePJ name)
        Crumb.Module   name      -> genCrumb GenCls.Module GenModule.ext $ GenModule.Module (encodePJ name)
        Crumb.Lambda   i         -> genCrumb GenCls.Lambda GenLambda.ext $ GenLambda.Lambda (encodePJ i)
        where
            genCrumb :: GenCls.Cls -> Extensions.Key Maybe Gen.Crumb v -> v -> Gen.Crumb
            genCrumb cls key ext = Extensions.putExt key (Just ext)
                                  $ Gen.Crumb cls $ Extensions.ExtField Map.empty
    decode t@(Gen.Crumb cls _) = case cls of
        GenCls.Function -> do
            GenFunction.Function name path <- getExt GenFunction.ext "Crumb.Function"
            Crumb.Function <$> decodeJ name (missing "Crumb.Function" "name")
                           <*> pure (decodeListP path)
        GenCls.Class    -> do
            GenClass.Class name <- getExt GenClass.ext "Crumb.Class"
            Crumb.Class <$> decodePJ name (missing "Crumb.Class" "name")
        GenCls.Module   -> do
            GenModule.Module name <- getExt GenModule.ext "Crumb.Module"
            Crumb.Module <$> decodePJ name (missing "Crumb.Module" "name")
        GenCls.Lambda   -> do
            GenLambda.Lambda i <- getExt GenLambda.ext "Crumb.Lambda"
            Crumb.Lambda <$> decodePJ i (missing "Crumb.Lambda" "id")
       where getExt key datatype = Extensions.getExt key t <?&> missing datatype "extension"


instance Convert Breadcrumbs Gen.Breadcrumbs where
    encode = Gen.Breadcrumbs . encodeList
    decode (Gen.Breadcrumbs b) = decodeList b


instance Convert (Breadcrumbs, Lib.ID) Gen.ASTPtr where
    encode (bc, libraryID) = Gen.ASTPtr (encodeJ bc) (encodePJ libraryID)
    decode (Gen.ASTPtr bc libraryID) =
        (,) <$> decodeJ  bc        (missing "ASTPtr" "breadcrumbs")
            <*> decodePJ libraryID (missing "ASTPtr" "libraryID"  )
