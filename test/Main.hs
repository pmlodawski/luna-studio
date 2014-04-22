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

import qualified AWS                          as AWS
import           AWS.EC2                      (EC2)
import qualified AWS.EC2.Types                as Types
import qualified AWS.EC2.Util                 as Util
import qualified Control.Concurrent           as Concurrent
import           Control.Monad                (forM)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import qualified Control.Monad.Loops          as Loops
import qualified Control.Monad.Trans.Resource as Resource
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Database.PostgreSQL.Simple   as PSQL
import           Text.Show.Pretty             (ppShow)

import qualified Flowbox.AWS.EC2               as EC2
import qualified Flowbox.AWS.Instance.Instance as Instance
import qualified Flowbox.AWS.Instance.Request  as Request
import           Flowbox.AWS.Region            (Region)
import qualified Flowbox.AWS.Region            as Region
import qualified Flowbox.AWS.User.Database     as Database
import           Flowbox.AWS.User.User         (User (User))
import           Flowbox.Prelude


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


queryDB :: IO ()
queryDB = do
    putStrLn "Connecting..."
    db <- Database.mk $ PSQL.ConnectInfo "mydbinstance.cn1bxyb5bfdl.eu-west-1.rds.amazonaws.com"
                                         5432
                                         "test"
                                         "************"
                                         "flowbox"
    putStrLn "Connected"
    Database.addUser db $ User "stefan" "ala123"
    Database.addUser db $ User "zenon"  "ala123"
    u1 <- Database.getUser db "stefan"
    u2 <- Database.getUser db "zenon"
    print u1
    print u2


main :: IO ()
main = do queryDB
          putStrLn "quiting"
