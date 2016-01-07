module Main where

import           Prologue
import qualified Data.ByteString          as ByteString
import qualified Data.ByteString.Char8    as Char8 (pack)

import qualified Flowbox.Config.Config    as Config
import qualified Flowbox.Bus.EndPoint     as EP
import qualified Flowbox.Bus.Bus          as Bus
import qualified Flowbox.Bus.Data.Flag    as Flag
import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Options.Applicative hiding (info)
import qualified Flowbox.Options.Applicative as Opt


data Cmd = Test1
         | Test2
         | Test3
         | Test4
         deriving Show

parser :: Parser Cmd
parser = Opt.flag' Test1 (short '1')
     <|> Opt.flag' Test2 (short '2')
     <|> Opt.flag' Test3 (short '3')
     <|> Opt.flag' Test4 (short '4')

run :: Cmd -> IO ()
run cmd = case cmd of
    Test1  -> test1
    Test2  -> test2
    Test3  -> test3
    Test4  -> test4


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header "ver 0.1")

main = execParser opts >>= run

-- tests

test1 :: IO ()
test1 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "empire.hello.request" (Char8.pack "test 1")
    return ()

test2 :: IO ()
test2 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "any-hello" ByteString.empty
    return ()

test3 :: IO ()
test3 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "empire.hello.request" (Char8.pack "dupa")
    return ()

test4 :: IO ()
test4 = do
    endPoints <- EP.clientFromConfig <$> Config.load
    Bus.runBus endPoints $ do
        Bus.send Flag.Enable $ Message.Message "empire.hello.request" (Char8.pack "dupa")
    return ()
