---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.AWS.EC2.Instance.Instance where

import qualified Data.List                            as List
import           Data.Text                            (Text)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)
import           Database.PostgreSQL.Simple.ToRow     (ToRow, toRow)

import qualified Flowbox.Data.Time as Time
import           Flowbox.Prelude   hiding (id)



type ID = Text


data Status = Running
            | Stopped
            | Other
            deriving (Show, Ord, Eq, Read)


data Instance = Instance { _id      :: ID
                         , _started :: Time.UTCTime
                         , _status  :: Status
                         } deriving (Show, Ord, Eq, Read)

makeLenses (''Instance)

---- instances ------------------------------------------------------------

instance FromField Status where
    fromField f dat = read <$> fromField f dat

instance ToField Status where
    toField = toField . show

instance FromRow Instance where
    fromRow = Instance <$> field <*> field <*> field

instance ToRow Instance where
    toRow (Instance id' started' status') =
        [toField id', toField started', toField status']

---- methods --------------------------------------------------------------

isRunning :: Instance -> Bool
isRunning inst = inst ^. status == Running


spareSeconds :: Time.UTCTime -> Instance -> Int
spareSeconds currentTime inst =
    (Time.toSeconds $ Time.diffUTCTime currentTime $ inst ^. started) `mod` 3600


sortByStatusAndTime :: Time.UTCTime -> [Instance] -> [Instance]
sortByStatusAndTime currentTime = List.sortBy comp where
    comp inst1 inst2 = case compare (inst1 ^. status) (inst2 ^. status) of
        EQ  -> case compare (spareSeconds currentTime inst1) (spareSeconds currentTime inst2) of
                EQ  -> compare inst1 inst2
                cmp -> cmp
        cmp -> cmp
