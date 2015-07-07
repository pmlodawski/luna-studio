---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

module Main where

import           Flowbox.Bus.Bus                    (Bus)
import qualified Flowbox.Bus.Bus                    as Bus
import qualified Flowbox.Bus.Data.Flag              as Flag
import qualified Flowbox.Bus.Data.Message           as Message
import qualified Flowbox.Bus.Data.MessageFrame      as MessageFrame
import qualified Flowbox.WSConnector.WSConnector    as WSConnector
import           Flowbox.Bus.EndPoint               (BusEndPoints (BusEndPoints))

import qualified System.ZMQ4.Monadic   as ZMQ
import qualified Data.ByteString.Char8 as Char8
import           Flowbox.Prelude

test :: Bus ()
test = do
    Bus.subscribe ""
    clientID <- Bus.getClientID
    print clientID
    Bus.send Flag.Enable  ( Message.Message "urm.undo.request" (Char8.pack "123") )
    putStrLn "sent"
    frame <- Bus.receive
    print $ "message: " ++ ( frame ^. MessageFrame.message . Message.topic )

    return ()


endPoints :: BusEndPoints
endPoints = BusEndPoints "tcp://127.0.0.1:30530"
                         "tcp://127.0.0.1:30531"
                         "tcp://127.0.0.1:30532"

main :: IO ()
main = do
    x <- ZMQ.runZMQ $ Bus.runBus endPoints test
    print x
    return ()