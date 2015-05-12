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

module Luna.DEP.Data.Serialize.Proto.Conversion.Library where

import           Control.Applicative
import           Data.Int            (Int32)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Library.LibManager                   as Gen
import qualified Generated.Proto.Dep.Library.Library                      as Gen
import qualified Generated.Proto.Dep.Library.Library.PropertyMap          as Gen
import qualified Generated.Proto.Dep.Library.Library.PropertyMap.KeyValue as Gen
import qualified Luna.DEP.AST.AST                                         as AST
import           Luna.DEP.Data.ASTInfo                                    (ASTInfo)
import qualified Luna.DEP.Data.ASTInfo                                    as ASTInfo
import           Luna.DEP.Data.Serialize.Proto.Conversion.Attributes      ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Module          ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Version         ()
import           Luna.DEP.Graph.Properties                                (Properties)
import           Luna.DEP.Lib.Lib                                         (Library (Library))
import qualified Luna.DEP.Lib.Lib                                         as Library
import           Luna.DEP.Lib.Manager                                     (LibManager)
import qualified Luna.DEP.Lib.Manager                                     as LibManager



instance ConvertPure ASTInfo Int32 where
    encodeP = encodeP . view ASTInfo.lastID
    decodeP = ASTInfo.mk . decodeP


instance ConvertPure Library.ID Int32 where
    encodeP = encodeP . Library.toInt
    decodeP = Library.ID . decodeP


instance Convert (Library.ID, Library) Gen.Library where
    encode (i, Library name version path ast propertyMap lastID) =
        Gen.Library (encodePJ i) (encodePJ name) (encodePJ version) (encodePJ path) (encodeJ ast) (encodeJ propertyMap) (encodeP lastID)
    decode (Gen.Library mtid mtname mtversion mtpath mtast mtpropertyMap lastID) = do
        i            <- decodeP <$> mtid   <?> "Failed to decode Library: 'id' field is missing"
        name         <- decodeP <$> mtname <?> "Failed to decode Library: 'name' field is missing"
        version      <- decodeP <$> mtversion <?> "Failed to decode Library: 'version' field is missing"
        path         <- decodeP <$> mtpath <?> "Failed to decode Library: 'path' field is missing"
        tpropertyMap <- mtpropertyMap <?> "Failed to decode Library: 'propertyMap' field is missing"
        tast         <- mtast   <?> "Failed to decode Library: 'ast' field is missing"
        ast          <- decode tast
        propertyMap  <- decode tpropertyMap
        pure (i, Library name version path ast propertyMap (decodeP lastID))


instance Convert (IntMap Properties) Gen.PropertyMap where
    encode pm = Gen.PropertyMap $ encode $ IntMap.toList pm
    decode (Gen.PropertyMap items) = IntMap.fromList <$> decode items


instance Convert (AST.ID, Properties) Gen.KeyValue where
    encode (i, p) = Gen.KeyValue (encodeP i) (encode p)
    decode (Gen.KeyValue ti tp) = do p <- decode tp
                                     return (decodeP ti, p)

instance Convert LibManager Gen.LibManager where
    encode = Gen.LibManager . encode . LibManager.labNodes
    decode (Gen.LibManager libs) = LibManager.mk <$> decode libs <*> pure []
