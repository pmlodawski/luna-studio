---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.FileManager.Item where

import qualified Flowbox.AWS.S3.Utils                            as S3
import           Flowbox.Prelude
import qualified Generated.Proto.FileManager.FileSystem.Item     as Gen
import qualified Generated.Proto.FileManager.FileSystem.Item.Cls as Gen


-- TODO [PM] : implement me

toGen :: FilePath -> Gen.Item
toGen path = Gen.Item cls Nothing Nothing Nothing Nothing where
    cls | S3.isDirectory path   = Gen.Directory
        | otherwise             = Gen.File
