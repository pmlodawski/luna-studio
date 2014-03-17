---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import qualified Data.List as List

import qualified Flowbox.Bus.Defaults        as Defaults
import           Flowbox.Bus.Logger.Cmd      (Cmd)
import qualified Flowbox.Bus.Logger.Cmd      as Cmd
import qualified Flowbox.Bus.Logger.Logger   as Logger
import qualified Flowbox.Bus.Logger.Version  as Version
import           Flowbox.Options.Applicative hiding (info)
import qualified Flowbox.Options.Applicative as Opt
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.Logger"


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Run
           <$> many      ( strOption ( short 't' <> metavar "TOPIC" <> help "topics to log"))
           <*> strOption ( long "ctrl-addr" <> short 'c' <> value Defaults.defaultCtrlEndPoint <> metavar "endpoint" <> help "Server control endpoint" )
           <*> strOption ( long "pull-addr" <> short 'l' <> value Defaults.defaultPullEndPoint <> metavar "endpoint" <> help "Server pull endpoint"    )
           <*> strOption ( long "pub-addr"  <> short 'b' <> value Defaults.defaultPubEndPoint  <> metavar "endpoint" <> help "Server publish endpoint" )
           <*> optIntFlag (Just "verbose") 'v' 2 3          "Verbose level (level range is 0-5, default level is 3)"
           <*> switch    ( long "no-color"          <> help "Disable color output" )


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))


main :: IO ()
main = execParser opts >>= run


run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version  -> putStrLn (Version.full False) -- TODO [PM] hardcoded numeric = False
    Cmd.Run {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        let topics = if List.null $ Cmd.topics cmd
                        then [""]
                        else Cmd.topics cmd
        r <- Logger.run (Cmd.endPoints cmd) topics
        case r of
            Left err -> logger criticalFail err
            _        -> return ()

