---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Options.Applicative         hiding (info)
import qualified Options.Applicative         as Opt
import           Data.List                     
import           Prelude                     hiding (error)
import qualified Flowbox.Data.Version        as Version 
import           Flowbox.Control.Applicative   
import qualified System.Environment          as Env
import           Flowbox.Control.Exception
import           Flowbox.System.Log.Logger     
import qualified Flowbox.System.Log.Logger   as Logger

logger :: Logger
logger = getLogger "Flowbox.Lunac"

libPathEnv :: String
libPathEnv = "LUNAPATH"

version :: Version.Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


data Conf = Compilation { inputs   :: [String]
                        , verbose  :: Bool
                        , noColor  :: Bool
                        , dump_ast :: Bool
                        }
          | Version
          deriving Show


parser :: Parser Conf
parser   = flag' Version (long "version" <> hidden)
       <|> Compilation
           <$> many1(argument str ( metavar "inputs" ))
           -- <*> strOption (long "verbose"  <> short 'v' <> value "0" <> help "Verbose level")
           <*> switch    (long "verbose"  <> short 'v'                 <> help "Verbose level")
           <*> switch    (long "no-color"                              <> help "Disable color output")
           <*> switch    (long "dump-ast" <> hidden)


opts :: ParserInfo Conf
opts = Opt.info (helper <*> parser)
           (Opt.fullDesc
               <> Opt.header show_version
           )


show_version :: String
show_version = "Luna compiler, version " ++ Version.str version


main :: IO ()
main = execParser opts >>= run


run :: Conf -> IO ()
run conf = case conf of
    Version     {} -> putStrLn show_version
    Compilation {} -> do
        case verbose conf of
            True  -> logger.setLevel $ DEBUG
            False -> logger.setLevel $ INFO

        case noColor conf of
            True   -> Logger.enableColorOutput False logger
            False  -> Logger.enableColorOutput True  logger

        logger.debug $ "Searching for environment variable '" ++ libPathEnv ++ "'"
        pushLogGroup logger
        lunapath <- try $ Env.getEnv libPathEnv
        case lunapath of
            Left _    -> logger.debug $ "Not defined"
            Right var -> logger.debug $ "Found: '" ++ var ++ "'"
        popLogGroup logger

        let inputs' = inputs conf
        logger.debug $ "Reading input files: " ++ show inputs'

        return ()
