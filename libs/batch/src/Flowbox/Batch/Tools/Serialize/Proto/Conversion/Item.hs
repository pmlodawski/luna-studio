---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item where

import qualified Text.ProtocolBuffers.Basic             as Proto

import           Flowbox.Prelude                          
import qualified Flowbox.Batch.FileSystem.Item          as Item
import           Flowbox.Batch.FileSystem.Item            (Item(..))
import           Flowbox.Control.Error                    
import qualified Flowbox.System.UniPath                 as UniPath
import           Flowbox.Tools.Conversion                 
import qualified Generated.Proto.FileSystem.FSItem      as PFS
import qualified Generated.Proto.FileSystem.FSItem.Type as PFS


instance Convert Item PFS.FSItem where
    encode item = PFS.FSItem (Just titemType) (Just tpath) (Just tsize) where 
        titemType :: PFS.Type
        titemType = case item of
            Directory {} -> PFS.Directory
            File      {} -> PFS.File
            Other     {} -> PFS.Other
        tpath = Proto.uFromString $ UniPath.toUnixString $ Item.path item
        tsize = itoi32 $ Item.size item
    decode (PFS.FSItem mtitemType mtpath mtsize) = do 
        titemType <- mtitemType <?> "Failed to decode Item: 'itemType' field is missing"
        tpath     <- mtpath     <?> "Failed to decode Item: 'path' field is missing"
        tsize     <- mtsize     <?> "Failed to decode Item: 'size' field is missing"
        let apath = UniPath.fromUnixString $ Proto.uToString tpath
            asize = i32toi tsize
        case titemType of 
            PFS.Directory -> return $ Directory apath asize
            PFS.File      -> return $ File      apath asize
            PFS.Other     -> return $ Other     apath asize