module Main where

import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as Char8
import qualified Flowbox.Bus.Bus       as Bus
import           Flowbox.Prelude
import qualified System.ZMQ4.Monadic   as ZMQ

import           Flowbox.Bus.Bus          (Bus)
import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Bus.EndPoint     (BusEndPoints (BusEndPoints))



test :: Bus ()
test = do
    Bus.subscribe ""
    clientID <- Bus.getClientID
    Bus.reply (Message.CorrelationID clientID 0) True
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

