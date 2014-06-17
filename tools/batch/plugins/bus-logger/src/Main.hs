---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import qualified Data.List as List

import qualified Flowbox.Bus.EndPoint        as EP
import           Flowbox.Bus.Logger.Cmd      (Cmd)
import qualified Flowbox.Bus.Logger.Cmd      as Cmd
import qualified Flowbox.Bus.Logger.Logger   as Logger
import qualified Flowbox.Bus.Logger.Version  as Version
import qualified Flowbox.Config.Config       as Config
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
        endPoints <- EP.clientFromConfig <$> Config.load
        let topics = if List.null $ Cmd.topics cmd
                        then [""]
                        else Cmd.topics cmd
        r <- Logger.run endPoints topics
        case r of
            Left err -> logger criticalFail err
            _        -> return ()
