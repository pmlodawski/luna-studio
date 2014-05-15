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

import qualified AWS                  as AWS
import qualified AWS.EC2.Types        as Types
import           Control.Monad.Reader
import           Data.Text            (Text)

import qualified Flowbox.AWS.EC2.EC2                    as EC2
import qualified Flowbox.AWS.EC2.Instance.Request       as Request
import qualified Flowbox.AWS.EC2.Pool.Instance.Instance as Instance
import qualified Flowbox.AWS.EC2.Pool.Pool              as Pool
import           Flowbox.AWS.Region                     (Region)
import qualified Flowbox.AWS.Region                     as Region
import qualified Flowbox.AWS.User.User                  as User
import qualified Flowbox.Control.Concurrent             as Concurrent
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger


rootLogger :: Logger
rootLogger = getLogger "Flowbox"



type Tag = Text
type UserName = Text
type InstanceID = Text


region :: Region
region = Region.fromText "eu-west-1"


main :: IO ()
main = do
    rootLogger setIntLevel 5
    credential <- AWS.loadCredential
    let userName = "zenon" :: User.Name
    EC2.runEC2InRegion credential region $ do
        mpool <- Pool.initialize
        thread <- liftIO $ Concurrent.forkIO' $ EC2.runEC2InRegion credential region $ Instance.monitor mpool

        ip    <- Types.instanceIpAddress <$> Instance.retrieve userName Request.mk mpool
        liftIO $ print ip

        liftIO $ Concurrent.threadDelay 5000000

        Instance.releaseUser userName mpool

        liftIO $ Concurrent.waitThread thread
