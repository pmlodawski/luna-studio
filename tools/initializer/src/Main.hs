---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Flowbox.Prelude                 hiding (error)
import qualified Flowbox.Config.Config           as Config
import qualified Flowbox.Data.Version            as Version
import           Flowbox.Data.Version              (Version)
import qualified Flowbox.Initializer.CmdArgs     as CmdArgs
import           Flowbox.Initializer.CmdArgs       (CmdArgs)
import qualified Flowbox.Initializer.Initializer as Initializer
import qualified Flowbox.Options.Applicative     as Opt
import           Flowbox.Options.Applicative     hiding (info)
import           Flowbox.System.Log.Logger         



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


version :: Version
version = Version.mk 
--{ Version.minor = 1
--                     , Version.stage = Version.Alpha
--                     }


parser :: Parser CmdArgs
parser = Opt.flag' CmdArgs.Version (long "version" <> hidden)
       <|> CmdArgs.Initialization
           <$> optIntFlag   "verbose" 'v' 2 3 "Verbose level (level range is 0-5, default level is 3)"
           <*> switch (long "force"   <> short 'f'                    <> help "Force reinitialization")



opts :: ParserInfo CmdArgs
opts = Opt.info (helper <*> parser)
           (Opt.fullDesc
               <> Opt.header show_version
           )


show_version :: String
show_version = "Luna initializer, version " ++ Version.str version


main :: IO ()
main = execParser opts >>= run


run :: CmdArgs -> IO ()
run cmd = case cmd of
    CmdArgs.Version     {} -> putStrLn show_version
    CmdArgs.Initialization {} -> do
        rootLogger setIntLevel $ CmdArgs.verbose cmd
        config <- Config.load
        if CmdArgs.force cmd
            then do Initializer.clear      config
                    Initializer.initialize config
            else Initializer.initializeIfNeeded config
