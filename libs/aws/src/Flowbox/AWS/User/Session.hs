---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Flowbox.AWS.User.Session where

import qualified Data.Time                            as Time
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.FromRow   (FromRow, field, fromRow)
import qualified Text.Read                            as Read

import qualified Flowbox.AWS.EC2.Instance.Instance              as Instance
import qualified Flowbox.AWS.User.User                          as User
import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Session.Policy                 as Gen
import qualified Generated.Proto.Session.Session                as Gen



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


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


instance ConvertPure Policy Gen.Policy where
    encodeP Halt       = Gen.Halt
    encodeP Autocharge = Gen.Autocharge
    decodeP Gen.Autocharge = Autocharge
    decodeP Gen.Halt       = Halt


instance Convert Session Gen.Session where
    encode (Session id' userName' instanceID' expires' policy') =
        Gen.Session (encodePJ id') (encodePJ userName') (encodePJ instanceID')
                    (encodePJ $ show expires') (encodePJ policy')
    decode (Gen.Session mid' muserName' minstanceID' mexpires' mpolicy') = do
        id'         <- decodeP <$> mid'         <?> "Failed to decode Session: 'id' field is missing"
        userName'   <- decodeP <$> muserName'   <?> "Failed to decode Session: 'userName' field is missing"
        instanceID' <- decodeP <$> minstanceID' <?> "Failed to decode Session: 'instanceID' field is missing"
        texpires'   <- decodeP <$> mexpires'    <?> "Failed to decode Session: 'expires' field is missing"
        policy'     <- decodeP <$> mpolicy'     <?> "Failed to decode Session: 'policy' field is missing"
        expires'    <- Read.readMaybe texpires' <?> "Failed to decode Session: 'expires' field is wrong"
        return $ Session id' userName' instanceID' expires' policy'

