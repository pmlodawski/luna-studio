---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Bus.Defaults where

import Flowbox.Prelude



defaultListenCtrlEndPoint :: String
defaultListenCtrlEndPoint = "tcp://*:30530"


defaultListenPullEndPoint :: String
defaultListenPullEndPoint = "tcp://*:30531"


defaultListenPubEndPoint :: String
defaultListenPubEndPoint  = "tcp://*:30532"



defaultCtrlEndPoint :: String
defaultCtrlEndPoint = "tcp://127.0.0.1:30530"


defaultPullEndPoint :: String
defaultPullEndPoint = "tcp://127.0.0.1:30531"


defaultPubEndPoint :: String
defaultPubEndPoint  = "tcp://127.0.0.1:30532"
