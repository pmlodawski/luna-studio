---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Hash where

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.Data.Hash (Hash)
import Luna.Interpreter.Session.Session   (Session)
import qualified Luna.Interpreter.Session.Session as Session



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Hash"


compute :: String -> Session (Maybe Hash)
compute varName = Session.interpret $ "hash " ++ varName
