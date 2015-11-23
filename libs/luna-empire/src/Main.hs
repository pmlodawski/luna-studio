module Main where

import           Prologue
import qualified Flowbox.Config.Config    as Config
import qualified Flowbox.Bus.EndPoint     as EP
import qualified Flowbox.Bus.Bus          as Bus
import qualified Flowbox.Bus.Data.Flag    as Flag
import qualified Flowbox.Bus.Data.Message as Message
import qualified Data.ByteString          as ByteString

main :: IO ()
main = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "hello" ByteString.empty
    return ()

