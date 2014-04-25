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

import qualified AWS                        as AWS
import qualified Aws                        as Aws
import qualified AWS.EC2.Types              as Types
import           Control.Monad.Reader
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple as PSQL
import qualified Flowbox.AWS.S3.S3          as S3

import qualified Flowbox.AWS.EC2.EC2                 as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance   as Instance
import qualified Flowbox.AWS.EC2.Instance.Request    as Request
import           Flowbox.AWS.Region                  (Region)
import qualified Flowbox.AWS.Region                  as Region
import qualified Flowbox.AWS.S3.File                 as File
import qualified Flowbox.AWS.User.Database           as Database
import           Flowbox.AWS.User.User               (User (User))
import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Flowbox.System.Console.ASCIISpinner as Spinner


type Tag = Text
type UserName = Text
type InstanceID = Text


region :: Region
region = Region.mk "eu-west-1"


getInstance :: IO ()
getInstance = do
    credential <- AWS.loadCredential
    let userName = "zenon"
    ip <- EC2.runEC2InRegion credential region
          $ Types.instanceIpAddress <$> Instance.getOrStart userName Request.mk
    print ip


queryDB :: IO (Either String ())
queryDB = runEitherT $ do
    putStr "Connecting "
    db <- liftIO $ Spinner.runWithSpinner $ Database.mk
            $ PSQL.ConnectInfo "mydbinstance.cn1bxyb5bfdl.eu-west-1.rds.amazonaws.com"
                               5432
                               "test"
                               "kozatest123"
                               "flowbox"
    --Database.create db
    Database.addUser db $ User "stefan" "ala123"
    Database.addUser db $ User "zenon"  "ala123"
    u1 <- liftIO $ Database.getUser db "stefan"
    u2 <- liftIO $ Database.getUser db "zenon"
    print u1
    print u2



s3Test :: IO ()
s3Test = Spinner.runWithSpinner $ do
    cfg <- Aws.baseConfiguration

    S3.runS3 cfg "flowbox-test1" $ do
        File.fetch  "images.jpeg"
        File.upload "images2.jpeg"
        File.touch  "images3.jpeg"
        File.exists "images2.jpeg" >>= liftIO . print
        File.rename "images2.jpeg" "images4.jpeg"

    return ()


main :: IO ()
main = do s3Test
          --r <- queryDB
          --print r
          putStrLn "quiting"

