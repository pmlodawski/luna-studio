---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Data.List                   as List
import           Options.Applicative         hiding (info)
import qualified Options.Applicative         as Opt

import           Flowbox.Prelude             hiding (error)
import           Flowbox.Control.Applicative   
import qualified Flowbox.Data.Version        as Version
import           Flowbox.Data.Version          (Version)
import qualified Flowbox.Lunac.Builder       as Builder
import qualified Flowbox.Lunac.Conf          as Conf
import           Flowbox.Lunac.Conf            (Conf)
import           Flowbox.System.Log.Logger     
import qualified Flowbox.System.UniPath      as UniPath
import           Flowbox.Lunac.Diagnostics     (Diagnostics(Diagnostics))



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


--logger :: Logger
--logger = getLogger "Flowbox.Lunac"


--libPathEnv :: String
--libPathEnv = "LUNAPATH"


version :: Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


parser :: Parser Conf
parser = Opt.flag' Conf.Version (long "version" <> hidden)
       <|> Conf.Compilation
           <$> many1     ( argument str ( metavar "inputs" ))
           -- <*> strOption ( long "verbose"  <> short 'v' <> value "0" <> help "Verbose level" )
           <*> strOption ( long "output"  <> short 'o' <> value "out" <> metavar "output" <> help "Output folder"        )
           <*> switch    ( long "verbose" <> short 'v'                                    <> help "Verbose level"        )
           <*> switch    ( long "no-color"                                                <> help "Disable color output" )
           <*> switch    ( long "dump-all"              <> hidden                                                        )
           <*> switch    ( long "dump-ast"              <> hidden                                                        )
           <*> switch    ( long "dump-va"               <> hidden                                                        )
           <*> switch    ( long "dump-ssa"              <> hidden                                                        )
           <*> switch    ( long "dump-hast"             <> hidden                                                        )
           <*> switch    ( long "dump-hsc"              <> hidden                                                        )


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
    Conf.Version     {} -> putStrLn show_version
    Conf.Compilation {} -> do
        if Conf.verbose conf
            then rootLogger setLevel DEBUG
            else rootLogger setLevel INFO

        let diag = Diagnostics ( Conf.dump_ast  conf || Conf.dump_all conf )
                               ( Conf.dump_va   conf || Conf.dump_all conf )
                               ( Conf.dump_ssa  conf || Conf.dump_all conf )
                               ( Conf.dump_hast conf || Conf.dump_all conf )
                               ( Conf.dump_hsc  conf || Conf.dump_all conf )
                               
            inputs = map UniPath.fromUnixString $ Conf.inputs conf
            outputPath = UniPath.fromUnixString $ Conf.output conf
            projectName = "project"
            tmpName = "tmp/tmp-333"

        Builder.initializeCabalDev

        sources <- mapM (Builder.buildFile diag) inputs
        Builder.buildSources tmpName $ List.concat sources
        Builder.runCabal tmpName projectName
        Builder.moveExecutable tmpName projectName outputPath
        Builder.cleanUp tmpName 


        --print $ length sources

        -- TODO [PM] : This code does not compile

        --case noColor conf of
        --    True   -> Logger.enableColorOutput False logger
        --    False  -> Logger.enableColorOutput True  logger

        --logger.debug $ "Searching for environment variable '" ++ libPathEnv ++ "'"

        --pushLogGroup logger 
        --lunapath <- try $ Env.getEnv libPathEnv
        --case lunapath of
        --    Left _    -> logger.debug $ "Not defined"
        --    Right var -> logger.debug $ "Found: '" ++ var ++ "'"
        --popLogGroup logger 

        --let inputs' = inputs conf
        --logger.debug $ "Reading input files: " ++ show inputs'

        return ()
