---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import Data.List
import System.IO
import Network
import System.Environment(getArgs)

import Data.Text.Lazy (pack) -- WTF

-- Thrift libraries
import Thrift
import Thrift.Transport.Handle
import Thrift.Protocol
import Thrift.Protocol.Binary
import Thrift.Server

-- Generated files
import Batch
import Batch_Iface

port :: PortNumber
port = 30521

data BatchHandler = BatchHandler

newBatchHandler = do
    return $ BatchHandler

instance Batch_Iface BatchHandler where
    ping a = putStrLn "ping received"

main :: IO ()
main = do
    handler <- newBatchHandler
    putStrLn "Starting the server..."
    runBasicServer handler Batch.process port
    putStrLn "done."