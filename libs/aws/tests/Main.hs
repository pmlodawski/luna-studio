---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import Flowbox.Prelude

import qualified AWS              as AWS
import qualified AWS.EC2          as EC2
import qualified AWS.EC2.Types    as Types
import qualified AWS.EC2.Util     as Util
import qualified Data.Conduit     as Conduit
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Text.Show.Pretty (ppShow)



type Tag = Text


userTag :: Tag
userTag = Text.pack "tag:user"


main :: IO ()
main = do
    credential <- AWS.loadCredential
    let runEC2   = Conduit.runResourceT . EC2.runEC2 credential
        userName = Text.pack "zenon"
        region   = Text.pack "eu-west-1"
    result <- runEC2 $ do EC2.setRegion region
                          instances <- Util.list $ EC2.describeInstances [] [(userTag, [userName])]
                          return $ (concatMap Types.reservationInstanceSet) instances
                          --return $ (Types.instanceIpAddress . head . Types.reservationInstanceSet . head) instances
    putStrLn $ ppShow result
    putStrLn "quiting"
