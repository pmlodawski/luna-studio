---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Options.Applicative             hiding (info)
import qualified Options.Applicative             as Opt

import           Flowbox.Prelude                 hiding (error)
import qualified Flowbox.Config.Config           as Config
import qualified Flowbox.Data.Version            as Version
import           Flowbox.Data.Version              (Version)
import qualified Flowbox.Initializer.CmdArgs     as CmdArgs
import           Flowbox.Initializer.CmdArgs       (CmdArgs)
import qualified Flowbox.Initializer.Initializer as Initializer
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
           <$> switch (long "verbose" <> short 'v' <> help "Verbose level")
           <*> switch (long "force"   <> short 'f' <> help "Force reinitialization")



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
run args = case args of
    CmdArgs.Version     {} -> putStrLn show_version
    CmdArgs.Initialization {} -> do
        if CmdArgs.verbose args
            then rootLogger setLevel DEBUG
            else rootLogger setLevel INFO

        config <- Config.load
        if CmdArgs.force args
            then do Initializer.clear      config
                    Initializer.initialize config
            else Initializer.initializeIfNeeded config
