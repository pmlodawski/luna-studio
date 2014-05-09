---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.S3.Utils where

import           Data.Text (Text)
import qualified Data.Text as Text

import           Flowbox.Prelude
import qualified Flowbox.System.FilePath as FilePath



isDirectory :: FilePath -> Bool
isDirectory path = last path == '/'


dirMarker :: FilePath
dirMarker = "/"


normaliseDir :: FilePath -> FilePath
normaliseDir dirPath = (FilePath.normalise' dirPath) ++ dirMarker



directoryPrefix :: FilePath -> Maybe Text
directoryPrefix filePath = case filePath of
    "." -> Nothing
    p   -> if isDirectory p
        then Just $ Text.pack p
        else Just $ Text.pack (p ++ "/")
