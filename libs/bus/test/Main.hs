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
--import     Flowbox.Text.ProtocolBuffers
--import qualified Generated.Proto.Urm.URM.Undo.Request as Proto



test :: Bus ()
test = do
    Bus.subscribe ""
    clientID <- Bus.getClientID
--    Bus.send Flag.Enable $ Message.Message "urm.undo.request" (123 :: Int)
--    Bus.send Flag.Enable $ Message.Message "urm.undo.request" $ messagePut' (678910 :: Int)
    Bus.reply (Message.CorrelationID clientID 0) Flag.Enable
              (Message.Message "urm.undo.perform.request" (Char8.pack ""))
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

