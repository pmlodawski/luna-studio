---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.ZMQ.Handlers.Common (
	zmqRunScript,
) where

import qualified System.ZMQ3.Monadic       as ZMQ3

import           Flowbox.Prelude           hiding (error)
import           Flowbox.Control.Error       
import           Flowbox.System.Log.Logger   


loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Common"


zmqRunScript :: Script a -> ZMQ3.ZMQ z a
zmqRunScript s = do
    e <- ZMQ3.liftIO $ runEitherT s
    case e of
        Left  m -> do 
        	loggerIO error m
        	fail m
        Right a -> return a
