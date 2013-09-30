---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Options.Applicative             hiding (info)
import qualified Options.Applicative             as Opt

import           Flowbox.Prelude                 hiding (error)
import qualified Flowbox.Data.Version            as Version
import           Flowbox.Data.Version              (Version)
import qualified Flowbox.Initializer.Conf        as Conf
import           Flowbox.Initializer.Conf          (Conf)
import qualified Flowbox.Initializer.Initializer as Initializer
import           Flowbox.System.Log.Logger         


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


version :: Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


parser :: Parser Conf
parser = Opt.flag' Conf.Version (long "version" <> hidden)
       <|> Conf.Initialization
           <$> switch (long "verbose" <> short 'v' <> help "Verbose level")



opts :: ParserInfo Conf
opts = Opt.info (helper <*> parser)
           (Opt.fullDesc
               <> Opt.header show_version
           )


show_version :: String
show_version = "Luna initializer, version " ++ Version.str version


main :: IO ()
main = execParser opts >>= run


run :: Conf -> IO ()
run conf = case conf of
    Conf.Version     {} -> putStrLn show_version
    Conf.Initialization {} -> do
        if Conf.verbose conf
            then rootLogger setLevel DEBUG
            else rootLogger setLevel INFO

        Initializer.checkedInitialize
