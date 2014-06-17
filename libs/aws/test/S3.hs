---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Aws                    as Aws
import           Control.Monad.IO.Class (liftIO)

import           Flowbox.AWS.Region                  (Region)
import qualified Flowbox.AWS.Region                  as Region
import qualified Flowbox.AWS.S3.Directory            as Directory
import qualified Flowbox.AWS.S3.File                 as File
import qualified Flowbox.AWS.S3.S3                   as S3
import           Flowbox.Prelude
import qualified Flowbox.System.Console.ASCIISpinner as Spinner


region :: Region
region = Region.fromText "eu-west-1"


main :: IO ()
main = Spinner.runWithSpinner $ do
    cfg <- Aws.baseConfiguration

    S3.runS3 cfg "flowbox-test1" $ do
        Directory.getContentsRecurisively (".") >>= liftIO . print
        File.fetch  "." "images.jpeg"
        File.create "dummydir/../images3.jpeg"
        File.exists "images.jpeg" >>= liftIO . print
        File.rename "images.jpeg" "images2.jpeg"
        File.copy "images2.jpeg" "images3.jpeg"
        Directory.create "emptydir1/"
        Directory.rename "emptydir1" "dir1"
        File.create "test/bla.txt"
        File.create "test/inner/bla.txt"
        Directory.create "test/inner2"
        Directory.getContents ("test/") >>= liftIO . print
        Directory.getContentsRecurisively (".") >>= liftIO . print
        Directory.fetch "tmp" "."
        Directory.remove "."
        Directory.upload "tmp" "."
        Directory.fetch "tmp2" "."
        Directory.copy "." "copy"
        Directory.rename "copy" "copy3"
        Directory.getContentsRecurisively (".") >>= liftIO . print
        Directory.remove "."
        File.upload "." "images.jpeg"
    return ()

