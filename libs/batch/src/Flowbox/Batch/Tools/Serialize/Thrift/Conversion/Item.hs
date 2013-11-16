---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Flowbox.Batch.Tools.Serialize.Thrift.Conversion.Item where

import           Data.Text.Lazy                                        (pack, unpack)

import           Flowbox.Prelude                                       
import qualified Flowbox.Batch.FileSystem.Item                       as Item
import           Flowbox.Batch.FileSystem.Item                         (Item(..))
import           Flowbox.Control.Error                                 
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs   ()
import qualified Flowbox.System.UniPath                              as UniPath
import           Flowbox.Tools.Conversion.Thrift                       
import qualified Fs_Types                                            as TFS


instance Convert Item TFS.FSItem where
    encode item = TFS.FSItem (Just titemType) (Just tpath) (Just tsize) where 
        titemType :: TFS.FSItemType
        titemType = case item of
            Directory {} -> TFS.Directory
            File      {} -> TFS.File
            Other     {} -> TFS.Other
        tpath = pack $ UniPath.toUnixString $ Item.path item
        tsize = itoi32 $ Item.size item
    decode (TFS.FSItem mtitemType mtpath mtsize) = do 
        titemType <- mtitemType <?> "Failed to decode Item: 'itemType' field is missing"
        tpath     <- mtpath     <?> "Failed to decode Item: 'path' field is missing"
        tsize     <- mtsize     <?> "Failed to decode Item: 'size' field is missing"
        let apath = UniPath.fromUnixString $ unpack tpath
            asize = i32toi tsize
        case titemType of 
            TFS.Directory -> return $ Directory apath asize
            TFS.File      -> return $ File      apath asize
            TFS.Other     -> return $ Other     apath asize