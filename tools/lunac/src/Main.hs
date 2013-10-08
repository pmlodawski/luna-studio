---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Data.List                       as List
import           Options.Applicative             hiding (info)
import qualified Options.Applicative             as Opt
import           Flowbox.Prelude                 hiding (error)
import           Flowbox.Control.Applicative       
import qualified Flowbox.Data.Version            as Version
import           Flowbox.Data.Version              (Version)
import qualified Flowbox.Initializer.Initializer as Initializer
import qualified Flowbox.Lunac.Builder           as Builder
import qualified Flowbox.Lunac.Conf              as Conf
import           Flowbox.Lunac.Conf                (Conf)
import           Flowbox.Lunac.Diagnostics         (Diagnostics(Diagnostics))
import           Flowbox.System.Log.Logger         
import qualified Flowbox.System.UniPath          as UniPath


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
           <*> many      ( strOption ( short 'l' <> metavar "LIBRARY" <> help "Library to link with.")                 )
           <*> strOption ( long "output"  <> short 'o' <> value "out"     <> metavar "OUTPUT"  <> help "Output folder" )
           <*> strOption ( long "name"    <> short 'n' <> value "name"    <> metavar "NAME"    <> help "Project name"  )
           <*> strOption ( long "root-path"            <> value ""        <> hidden                                    )
       
           <*> switch    ( long "global"                         <> help "Enable to compile to global cabal repository")
           <*> switch    ( long "library"                        <> help "Enable to compile as a library"              )
       
           <*> switch    ( long "verbose" <> short 'v'           <> help "Verbose level"                               )
           <*> switch    ( long "no-color"                       <> help "Disable color output"                        )

           <*> switch    ( long "dump-all"              <> hidden                                                      )
           <*> switch    ( long "dump-ast"              <> hidden                                                      )
           <*> switch    ( long "dump-va"               <> hidden                                                      )
           <*> switch    ( long "dump-fp"               <> hidden                                                      )
           <*> switch    ( long "dump-ssa"              <> hidden                                                      )
           <*> switch    ( long "dump-hast"             <> hidden                                                      )
           <*> switch    ( long "dump-hsc"              <> hidden                                                      )


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
                               ( Conf.dump_fp   conf || Conf.dump_all conf )
                               ( Conf.dump_ssa  conf || Conf.dump_all conf )
                               ( Conf.dump_hast conf || Conf.dump_all conf )
                               ( Conf.dump_hsc  conf || Conf.dump_all conf )
                               
            inputs = map UniPath.fromUnixString $ Conf.inputs conf

        Initializer.initializeIfNeeded

        mapM_ (Builder.buildFile conf diag) inputs
      