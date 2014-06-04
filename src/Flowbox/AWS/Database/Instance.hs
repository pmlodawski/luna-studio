---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.Instance where

import qualified Data.Maybe                 as Maybe
import           Database.PostgreSQL.Simple ((:.) ((:.)))
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.Instance.Add           as InstanceAdd
import qualified Flowbox.AWS.Database.SQL.Instance.All           as InstanceAll
import qualified Flowbox.AWS.Database.SQL.Instance.Delete        as InstanceDelete
import qualified Flowbox.AWS.Database.SQL.Instance.Find          as InstanceFind
import qualified Flowbox.AWS.Database.SQL.Instance.FindAvailable as InstanceFindAvailable
import qualified Flowbox.AWS.Database.SQL.Instance.FindFree      as InstanceFindFree
import qualified Flowbox.AWS.Database.SQL.Instance.Update        as InstanceUpdate
import           Flowbox.AWS.EC2.Instance.Instance               (Instance)
import qualified Flowbox.AWS.EC2.Instance.Instance               as Instance
import qualified Flowbox.AWS.User.User                           as User
import           Flowbox.Prelude



maxSessions :: Int
maxSessions = 2

---------------------------------------------------------------------------

add :: PSQL.Connection -> Instance -> IO ()
add conn inst =
    void $ PSQL.execute conn InstanceAdd.query inst


find :: PSQL.Connection -> Instance.ID -> IO (Maybe Instance)
find conn instanceID = Maybe.listToMaybe
    <$> PSQL.query conn InstanceFind.query (PSQL.Only instanceID)


delete :: PSQL.Connection -> [Instance.ID] -> IO ()
delete conn instanceIDs =
    void $ PSQL.execute conn InstanceDelete.query $ PSQL.Only $ PSQL.In instanceIDs


update :: PSQL.Connection -> Instance -> IO ()
update conn inst = do
    void $ PSQL.execute conn InstanceUpdate.query
         $ inst :. (PSQL.Only $ inst ^. Instance.id)


findWithAtMostUsers :: PSQL.Connection -> Int -> IO [Instance]
findWithAtMostUsers conn count =
    PSQL.query conn InstanceFindFree.query (PSQL.Only count)


findAvailable :: PSQL.Connection -> User.Name ->  IO [Instance]
findAvailable conn userName = 
    PSQL.query conn InstanceFindAvailable.query (maxSessions - 1, userName)


all :: PSQL.Connection -> IO [Instance]
all conn = PSQL.query conn InstanceAll.query ()
