---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.AWS.EC2.Instance.Instance where

import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)
import           Database.PostgreSQL.Simple.ToRow     (ToRow, toRow)
import qualified Flowbox.Data.Time                    as Time
import           Flowbox.Prelude                      hiding (id)


type ID = Text


data Status = Running
            | Stopped
            deriving (Show, Ord, Eq, Read)


data Instance = Instance { _id      :: ID
                         , _started :: Time.UTCTime
                         , _status  :: Status
                         } deriving (Show, Ord, Eq, Read)

makeLenses (''Instance)


instance FromField Status where
    fromField f dat = read <$> fromField f dat

instance ToField Status where
    toField = toField . show

instance FromRow Instance where
    fromRow = Instance <$> field <*> field <*> field

instance ToRow Instance where
    toRow (Instance id' started' status') =
        [toField id', toField started', toField status']
