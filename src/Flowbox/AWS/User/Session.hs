---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.AWS.User.Session where

import qualified Data.Time                            as Time
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)

import qualified Flowbox.AWS.EC2.Instance.Instance as Instance
import qualified Flowbox.AWS.User.User             as User
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.AWS.Session"


type ID = Int


data Policy = Autocharge
            | Halt
            deriving (Show, Read, Eq, Ord)

instance FromField Policy where
    fromField f dat = read <$> fromField f dat


data Session = Session { _id         :: ID
                       , _userName   :: User.Name
                       , _instanceID :: Instance.ID
                       , _expires    :: Time.UTCTime
                       , _policy     :: Policy
                       } deriving (Show, Read, Eq, Ord)

makeLenses (''Session)


instance FromRow Session where
    fromRow = Session <$> field <*> field <*> field <*> field <*> field
