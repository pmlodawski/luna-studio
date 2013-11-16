---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item where

import qualified Text.ProtocolBuffers.Basic     as Proto

import           Flowbox.Prelude                  
import qualified Flowbox.Batch.FileSystem.Item  as Item
import           Flowbox.Batch.FileSystem.Item    (Item(..))
import           Flowbox.Control.Error            
import qualified Flowbox.System.UniPath         as UniPath
import           Flowbox.Tools.Conversion.Proto   
import qualified Generated.Proto.FSItem         as Gen
import qualified Generated.Proto.FSItem.Type    as Gen



instance Convert Item Gen.FSItem where
    encode item = Gen.FSItem (Just titemType) (Just tpath) (Just tsize) where 
        titemType :: Gen.Type
        titemType = case item of
            Directory {} -> Gen.Directory
            File      {} -> Gen.File
            Other     {} -> Gen.Other
        tpath = Proto.uFromString $ UniPath.toUnixString $ Item.path item
        tsize = itoi32 $ Item.size item
    decode (Gen.FSItem mtitemType mtpath mtsize) = do 
        titemType <- mtitemType <?> "Failed to decode Item: 'itemType' field is missing"
        tpath     <- mtpath     <?> "Failed to decode Item: 'path' field is missing"
        tsize     <- mtsize     <?> "Failed to decode Item: 'size' field is missing"
        let apath = UniPath.fromUnixString $ Proto.uToString tpath
            asize = i32toi tsize
        case titemType of 
            Gen.Directory -> return $ Directory apath asize
            Gen.File      -> return $ File      apath asize
            Gen.Other     -> return $ Other     apath asize