{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Prologue
import qualified Data.List                   as List

import qualified Flowbox.Bus.EndPoint        as EP
import           Empire.Cmd                  (Cmd)
import qualified Empire.Cmd                  as Cmd
import qualified Empire.Server               as Server
import qualified Empire.Version              as Version
import qualified Flowbox.Config.Config       as Config
import           Flowbox.Options.Applicative (short, long, help, metavar)
import qualified Flowbox.Options.Applicative as Opt
import           Flowbox.System.Log.Logger

defaultTopic :: String
defaultTopic = "empire."

rootLogger :: Logger
rootLogger = getLogger ""

logger :: LoggerIO
logger = getLoggerIO $moduleName

parser :: Opt.Parser Cmd
parser = Opt.flag' Cmd.Version (short 'V' <> long "version" <> help "Version information")
       <|> Cmd.Run
           <$> Opt.many         (Opt.strOption (short 't' <> metavar "TOPIC" <> help "Topic to listen"))
           <*> Opt.optIntFlag   (Just "verbose") 'v' 2 3 "Verbosity level (0-5, default 3)"
           <*> not . Opt.switch (long "unformatted" <> help "Unformatted output" )

opts :: Opt.ParserInfo Cmd
opts = Opt.info (Opt.helper <*> parser)
                (Opt.fullDesc <> Opt.header Version.fullVersion)

main :: IO ()
main = Opt.execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn Version.fullVersion
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        endPoints <- EP.clientFromConfig <$> Config.load
        let topics = if List.null $ Cmd.topics cmd
                        then [defaultTopic]
                        else Cmd.topics cmd
            formatted = Cmd.formatted cmd
        r <- Server.run endPoints topics formatted
        case r of
            Left err -> logger criticalFail err
            _        -> return ()
