---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

module Flowbox.Batch.Tools.Serialize.Proto.Conversion.Item where

import           Flowbox.Prelude                                  
import qualified Flowbox.Batch.FileSystem.Item                  as Item
import           Flowbox.Batch.FileSystem.Item                    (Item(..))
import           Flowbox.Control.Error                            
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic   
import qualified Generated.Proto.Filesystem.Item                as Gen
import qualified Generated.Proto.Filesystem.Item.Type           as Gen



instance Convert Item Gen.Item where
    encode item = Gen.Item titemType tpath tsize where 
        titemType = Just $ case item of
            Directory {} -> Gen.Directory
            File      {} -> Gen.File
            Other     {} -> Gen.Other
        tpath = encodePJ $ Item.path item
        tsize = encodePJ $ Item.size item
    decode (Gen.Item mtitemType mtpath mtsize) = do 
        titemType <- mtitemType <?> "Failed to decode Item: 'itemType' field is missing"
        tpath     <- mtpath     <?> "Failed to decode Item: 'path' field is missing"
        tsize     <- mtsize     <?> "Failed to decode Item: 'size' field is missing"
        let apath = decodeP tpath
            asize = decodeP tsize
        case titemType of 
            Gen.Directory -> return $ Directory apath asize
            Gen.File      -> return $ File      apath asize
            Gen.Other     -> return $ Other     apath asize