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

import qualified AWS           as AWS
import qualified AWS.EC2.Types as Types
import           Data.Text     (Text)

import qualified Flowbox.AWS.EC2.Control.Simple.Instance as Simple
import qualified Flowbox.AWS.EC2.EC2                     as EC2
import qualified Flowbox.AWS.EC2.Instance.Request        as Request
import           Flowbox.AWS.Region                      (Region)
import qualified Flowbox.AWS.Region                      as Region
import qualified Flowbox.AWS.User.User                   as User
import           Flowbox.Prelude



type Tag = Text
type UserName = Text
type InstanceID = Text


region :: Region
region = Region.fromText "eu-west-1"


main :: IO ()
main = do
    credential <- AWS.loadCredential
    let userName = "zenon" :: User.Name
    ip <- EC2.runEC2inRegion credential region
          $ map Types.instanceIpAddress <$> Simple.getOrStartWait userName Request.mk
    print ip

