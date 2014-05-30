---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Flowbox.AWS.EC2.Instance.Tag where

import qualified AWS.EC2.Types as Types
import qualified Data.List     as List
import qualified Data.Time     as Time

import           Flowbox.AWS.EC2.EC2               (EC2, EC2Resource)
import qualified Flowbox.AWS.EC2.EC2               as EC2
import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import           Flowbox.AWS.Tag                   (Tag)
import qualified Flowbox.AWS.Tag                   as Tag
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Control.Error             (assert)
import           Flowbox.Prelude                   hiding (filter)




userTagKey :: Tag.Key
userTagKey = "user"


startTimeTagKey :: Tag.Key
startTimeTagKey = "start-time"


noUser :: Tag.Value
noUser = "_"


getStartTime :: Types.Instance -> Maybe Time.UTCTime
getStartTime inst = do
    let findStartTimeTag = List.find (\tag' -> Types.resourceTagKey tag' == startTimeTagKey)
    tagVal <- Types.resourceTagValue <$> (findStartTimeTag $ Types.instanceTagSet inst)
    read . Tag.unpack <$> tagVal


getUser :: Types.Instance -> Maybe User.Name
getUser inst = do
    let findUserTag = List.find (\tag' -> Types.resourceTagKey tag' == userTagKey)
    tagVal <- Types.resourceTagValue <$> (findUserTag $ Types.instanceTagSet inst)
    userName <- tagVal
    if userName == noUser
        then Nothing
        else return $ Tag.unpack userName


userTag :: Maybe User.Name -> Tag
userTag userName = (userTagKey, userTagValue) where
    userTagValue = case userName of
                     Just name -> Tag.pack $ name
                     Nothing   -> noUser


startTimeTag :: Time.UTCTime -> Tag
startTimeTag startTime = (startTimeTagKey, Tag.pack $ show startTime)


tag :: EC2Resource m => [Tag] -> [Instance.ID] -> EC2 m ()
tag tags instanceIDs =
    EC2.createTags instanceIDs tags >>= (`assert` "Failed to create tag")


userFilter :: User.Name -> [Types.Filter]
userFilter userName = filter userTagKey [Tag.pack userName]


filter :: Tag.Key -> [Tag.Value] -> [Types.Filter]
filter tagKey tagValues = [(Tag.append (Tag.pack "tag:") tagKey, tagValues)]
