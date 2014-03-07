---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Env where

import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers       as Proto



type ClientID = Proto.Int32

type EndPoint = String

data BusEndPoints = BusEndPoints { controlEndPoint :: EndPoint
                                 , pullEndPoint    :: EndPoint
                                 , pubEndPoint     :: EndPoint
                                 } deriving (Read, Show, Eq)

data BusEnv = BusEnv { endPoints :: BusEndPoints
                     , clientID  :: ClientID
                     } deriving (Read, Show, Eq)


