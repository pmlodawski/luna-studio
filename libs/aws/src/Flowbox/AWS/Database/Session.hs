---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.AWS.Database.Session where

import           Control.Monad
import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Time                  as Time
import qualified Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.SQL.Session.Add            as SessionAdd
import qualified Flowbox.AWS.Database.SQL.Session.Delete         as SessionDelete
import qualified Flowbox.AWS.Database.SQL.Session.DeleteByID     as SessionDeleteByID
import qualified Flowbox.AWS.Database.SQL.Session.FindByInstance as SessionFindByInstance
import qualified Flowbox.AWS.Database.SQL.Session.FindByUser     as SessionFindByUser
import qualified Flowbox.AWS.Database.SQL.Session.Update         as SessionUpdate
import qualified Flowbox.AWS.EC2.Instance.Instance               as Instance
import           Flowbox.AWS.User.Session                        (Session (Session))
import qualified Flowbox.AWS.User.Session                        as Session
import qualified Flowbox.AWS.User.User                           as User
import           Flowbox.Prelude                                 hiding (id)



create :: PSQL.Connection -> User.Name -> Instance.ID -> Time.UTCTime -> Session.Policy
       -> IO Session
create connection userName instanceID expires policy = do
    PSQL.Only id <- head <$> PSQL.query connection (fromString SessionAdd.query) (userName, instanceID, expires, show policy)
    return $ Session id userName instanceID expires policy


findByInstance :: PSQL.Connection -> Instance.ID -> IO [Session]
findByInstance connection instanceID = map fromDB <$> PSQL.query connection
    (fromString SessionFindByInstance.query) (PSQL.Only instanceID)


findByUser :: PSQL.Connection -> User.Name -> IO [Session]
findByUser connection userName = map fromDB <$> PSQL.query connection
    (fromString SessionFindByUser.query) (PSQL.Only userName)


deleteByID :: PSQL.Connection -> Session.ID -> IO ()
deleteByID connection sessionID =
    void $ PSQL.execute connection (fromString SessionDeleteByID.query) (PSQL.Only sessionID)


delete :: PSQL.Connection -> User.Name -> Instance.ID -> IO ()
delete connection userName instanceID =
    void $ PSQL.execute connection (fromString SessionDelete.query) (userName, instanceID)


update :: PSQL.Connection -> Session -> IO ()
update connection session = do
    void $ PSQL.execute connection (fromString SessionUpdate.query)
         (session ^. Session.expires, show $ session ^. Session.policy, session ^. Session.id)


fromDB :: (Int, String, Text, Time.UTCTime, String) -> Session
fromDB (id', userName', instanceID', expires', policy') =
    Session id' userName' instanceID' expires' (read policy')
