---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2015
---------------------------------------------------------------------------

module Main where

import qualified Flowbox.WSConnector.WSConnector as WSConnector
import           Flowbox.WSConnector.Cmd         (Cmd(..))
import           Flowbox.System.Log.Logger
import           Flowbox.Prelude
import           Flowbox.Options.Applicative     hiding (info)
import qualified Flowbox.Options.Applicative     as Opt
import qualified Flowbox.WSConnector.Version     as Version
import qualified Flowbox.Config.Config           as Config
import qualified FlowboxData.Config.Config       as WSConfigLoader
import qualified Flowbox.WSConnector.WSConfig    as WSConfig
import qualified Flowbox.Bus.EndPoint            as EndPoint

rootLogger :: Logger
rootLogger = getLogger ""

parser :: Parser Cmd
parser = Opt.flag' Version (long "version" <> hidden)
       <|> Run <$> optIntFlag (Just "verbose") 'v' 2 3 "Verbose level (level range is 0-5, default is 3)"

opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))

main :: IO ()
main = execParser opts >>= run

run :: Cmd -> IO ()
run cmd = case cmd of
    Version     -> putStrLn (Version.full False)
    Run verbose -> do
        busEndPoints <- EndPoint.clientFromConfig <$> Config.load
        config <- WSConfig.readWebsocketConfig <$> WSConfigLoader.load
        rootLogger setIntLevel verbose
        WSConnector.run busEndPoints config
