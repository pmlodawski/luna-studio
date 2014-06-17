module Main where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as Char8
import qualified System.ZMQ4.Monadic   as ZMQ

import           Flowbox.Bus.Bus          (Bus)
import qualified Flowbox.Bus.Bus          as Bus
import qualified Flowbox.Bus.Data.Flag    as Flag
import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Bus.EndPoint     (BusEndPoints (BusEndPoints))
import           Flowbox.Prelude



test :: Bus ()
test = do
    Bus.subscribe ""
    clientID <- Bus.getClientID
    Bus.reply (Message.CorrelationID clientID 0) Flag.Enable
              (Message.Message "project.open.request" (Char8.pack "some data"))
    putStrLn "sent"
    _ <- Bus.receive
    putStrLn "received"
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

