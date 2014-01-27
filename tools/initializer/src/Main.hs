---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

import qualified Flowbox.Config.Config           as Config
import           Flowbox.Initializer.Cmd         (Cmd)
import qualified Flowbox.Initializer.Cmd         as Cmd
import qualified Flowbox.Initializer.Initializer as Initializer
import qualified Flowbox.Initializer.Version     as Version
import           Flowbox.Options.Applicative     hiding (info)
import qualified Flowbox.Options.Applicative     as Opt
import           Flowbox.Prelude                 hiding (error)
import           Flowbox.System.Log.Logger


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


parser :: Parser Cmd
parser = Opt.flag' Cmd.Version (long "version" <> hidden)
       <|> Cmd.Initialization
           <$> optIntFlag (Just "verbose") 'v' 2 3 "Verbose level (level range is 0-5, default level is 3)"
           <*> switch (long "force"  <> short 'f' <> help "Force reinitialization")


opts :: ParserInfo Cmd
opts = Opt.info (helper <*> parser)
                (Opt.fullDesc <> Opt.header (Version.full False))


main :: IO ()
main = execParser opts >>= run


run :: Cmd -> IO ()
run cmd = case cmd of
    Cmd.Version           -> putStrLn (Version.full False) -- TODO [PM] hardcoded numeric = False
    Cmd.Initialization {} -> do
        rootLogger setIntLevel $ Cmd.verbose cmd
        cfg <- Config.load
        if Cmd.force cmd
            then do Initializer.clear      cfg
                    Initializer.initialize cfg
            else Initializer.initializeIfNeeded cfg
