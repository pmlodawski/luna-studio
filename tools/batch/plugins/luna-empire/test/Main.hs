module Main where

import           Prologue
import qualified Data.ByteString          as ByteString

import qualified Flowbox.Config.Config    as Config
import qualified Flowbox.Bus.EndPoint     as EP
import qualified Flowbox.Bus.Bus          as Bus
import qualified Flowbox.Bus.Data.Flag    as Flag
import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Options.Applicative hiding (info)
import qualified Flowbox.Options.Applicative as Opt


data Cmd = Test1
         | Test2
         deriving Show


test1 :: IO ()
test1 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "empire.hello" ByteString.empty
    return ()

test2 :: IO ()
test2 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "any-hello" ByteString.empty
    return ()


parser :: Parser Cmd
parser = Opt.flag' Test1 (short '1' <> help "Test 1")
     <|> Opt.flag' Test2 (short '2' <> help "Test 2")

opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header "ver 0.1")

main = execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Test1  -> test1
    Test2  -> test2
