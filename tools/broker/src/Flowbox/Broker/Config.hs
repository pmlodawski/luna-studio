---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Broker.Config (
    module Paths_flowbox_broker,
    defaultCtrlEndPoint,
    defaultPullEndPoint,
    defaultPubEndPoint,
) where

import Paths_flowbox_broker (version)

import Flowbox.Prelude



defaultCtrlEndPoint :: String
defaultCtrlEndPoint = "tcp://*:30530"


defaultPullEndPoint :: String
defaultPullEndPoint = "tcp://*:30531"


defaultPubEndPoint :: String
defaultPubEndPoint  = "tcp://*:30532"
