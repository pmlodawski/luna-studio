---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Text.Show.Pretty             (ppShow)

import qualified Flowbox.AWS.EC2      as EC2
import qualified Flowbox.AWS.Instance as Instance
import           Flowbox.AWS.Region   (Region)
import qualified Flowbox.AWS.Region   as Region
import           Flowbox.Prelude


type Tag = Text
type UserName = Text
type InstanceID = Text


region :: Region
region = Region.mk "eu-west-1"


main :: IO ()
main = do
    credential <- AWS.loadCredential
    let userName = "zenon"
    ip <- EC2.runEC2inRegion credential region
          $ Types.instanceIpAddress <$> Instance.get userName Instance.defaultInstanceRequest
    print ip
    putStrLn "quiting"
