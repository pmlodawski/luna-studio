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

module Luna.DEP.Data.Serialize.Proto.Conversion.Crumb where

import           Control.Applicative
import qualified Data.Map            as Map

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Crumb.ASTPtr                 as Gen
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs            as Gen
import qualified Generated.Proto.Dep.Crumb.Class                  as GenClass
import qualified Generated.Proto.Dep.Crumb.Crumb                  as Gen
import qualified Generated.Proto.Dep.Crumb.Crumb.Cls              as GenCls
import qualified Generated.Proto.Dep.Crumb.Function               as GenFunction
import qualified Generated.Proto.Dep.Crumb.Lambda                 as GenLambda
import qualified Generated.Proto.Dep.Crumb.Module                 as GenModule
import           Luna.DEP.AST.Control.Crumb                       (Breadcrumbs, Crumb)
import qualified Luna.DEP.AST.Control.Crumb                       as Crumb
import           Luna.DEP.Data.Serialize.Proto.Conversion.Library ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Name    ()
import qualified Luna.DEP.Lib.Lib                                 as Lib
import qualified Text.ProtocolBuffers.Extensions                  as Extensions



instance Convert Crumb Gen.Crumb where
    encode crumb = case crumb of
        Crumb.Function name path -> genCrumb GenCls.Function GenFunction.ext $ GenFunction.Function
                                      (encodeJ name) (encodeP path)
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
                           <*> pure (decodeP path)
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
    encode = Gen.Breadcrumbs . encode
    decode (Gen.Breadcrumbs b) = decode b


instance Convert (Breadcrumbs, Lib.ID) Gen.ASTPtr where
    encode (bc, libraryID) = Gen.ASTPtr (encodeJ bc) (encodePJ libraryID)
    decode (Gen.ASTPtr bc libraryID) =
        (,) <$> decodeJ  bc        (missing "ASTPtr" "breadcrumbs")
            <*> decodePJ libraryID (missing "ASTPtr" "libraryID"  )
