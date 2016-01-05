{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prologue
import qualified Data.List                   as List

import qualified Flowbox.Bus.EndPoint        as EP
import           Empire.Cmd                  (Cmd)
import qualified Empire.Cmd                  as Cmd
import qualified Empire.Logger               as Logger
import qualified Empire.Version              as Version
import qualified Flowbox.Config.Config       as Config
import           Flowbox.Options.Applicative hiding (info)
import qualified Flowbox.Options.Applicative as Opt
import           Flowbox.System.Log.Logger

defaultTopic :: String
defaultTopic = "empire."

rootLogger :: Logger
rootLogger = getLogger ""

logger :: LoggerIO
logger = getLoggerIO $moduleName

parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (short 'V' <> long "version" <> help "Version information")
       <|> Cmd.Run
           <$> many       (strOption (short 't' <> metavar "TOPIC" <> help "Topic to listen"))
           <*> optIntFlag (Just "verbose") 'v' 2 3 "Verbosity level (0-5, default 3)"

opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- EP.clientFromConfig <$> Config.load
        let topics = if List.null $ Cmd.topics cmd
                        then [defaultTopic]
                        else Cmd.topics cmd
        r <- Logger.run endPoints topics
        case r of
            Left err -> logger criticalFail err
            _        -> return ()
