---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.Instance where

import           Control.Monad
import qualified Data.Maybe                 as Maybe
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Time                  as Time
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.Instance.Add      as InstanceAdd
import qualified Flowbox.AWS.Database.SQL.Instance.Delete   as InstanceDelete
import qualified Flowbox.AWS.Database.SQL.Instance.Find     as InstanceFind
import qualified Flowbox.AWS.Database.SQL.Instance.FindFree as InstanceFindFree
import qualified Flowbox.AWS.Database.SQL.Instance.Update   as InstanceUpdate
import           Flowbox.AWS.EC2.Instance.Instance          (Instance (Instance))
import qualified Flowbox.AWS.EC2.Instance.Instance          as Instance
import qualified Flowbox.Data.Tuple                         as Tuple
import           Flowbox.Prelude



maxSessions :: Int
maxSessions = 2

-----------------------------------------------------------------------------

add :: PSQL.Connection -> Instance -> IO ()
add connection inst =
    void $ PSQL.execute connection (fromString InstanceAdd.query) $ toDB inst


find :: PSQL.Connection -> Instance.ID -> IO (Maybe Instance)
find connection instanceID = fmap fromDB . Maybe.listToMaybe
    <$> PSQL.query connection (fromString InstanceFind.query) (PSQL.Only instanceID)


delete :: PSQL.Connection -> Instance.ID -> IO ()
delete connection instanceID =
    void $ PSQL.execute connection (fromString InstanceDelete.query) (PSQL.Only instanceID)


update :: PSQL.Connection -> Instance -> IO ()
update connection inst = do
    void $ PSQL.execute connection (fromString InstanceUpdate.query)
         $ Tuple.add4and1 (toDB inst) (inst ^. Instance.id)


findFree :: PSQL.Connection -> IO [Instance]
findFree connection = map fromDB
    <$> PSQL.query connection (fromString InstanceFindFree.query) (PSQL.Only maxSessions)


-----------------------------------------------------------------------------

fromDB :: (Text, String, Time.UTCTime, String) -> Instance
fromDB (id', ip_addr', started', status') =
    Instance id' (read ip_addr') started' (read status')


toDB :: Instance -> (Text, String, Time.UTCTime, String)
toDB (Instance id' ip_addr' started' status') =
    (id', show ip_addr', started', show status')

