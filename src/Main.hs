---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Data.Version                  (Version(Version))
import qualified Options.Applicative         as Opt
import           Options.Applicative           ( (<>), argument, fullDesc, header, hidden, metavar
                                               , option, str, subparser, switch
                                               , long, help, strOption, value
                                               , short, command, progDesc, command)

import           Flowbox.Prelude               
import qualified Flowbox.Config.Config       as Config
import           Flowbox.Control.Applicative   
import qualified Flowbox.Lunac.Build         as Build
import qualified Flowbox.Lunac.Cmd           as Cmd
import qualified Flowbox.Lunac.Version       as Version
import           Flowbox.Options.Applicative   (optIntFlag)
import           Flowbox.System.Log.Logger     
                 


rootLogger :: Logger
rootLogger = getLogger "Flowbox"


versionParser :: Opt.Parser Cmd.Command
versionParser = Cmd.Version <$> (Cmd.VersionOptions <$> switch ( long "compiler" <> help "Print only the Luna compiler version" )
                                                    <*> switch ( long "library"  <> help "Print only the Luna library version" )
                                                    <*> switch ( long "numeric"  <> help "Print only numeric version" )
                                )

buildParser :: Opt.Parser Cmd.Command
buildParser = Cmd.Build <$> ( Cmd.BuildOptions <$> argument str ( metavar "INPUTS" )
                                               <*> strOption ( long "output"  <> short 'o' <> value "out"  <> metavar "OUTPUT"  <> help "output file name" )
                                               <*> optIntFlag Nothing 'O' 0 2 "optimisation level [0-2], default 2"
                                               <*> many      ( strOption ( short 'l' <> metavar "LIBRARY" <> help "Library to link with."))
                                              
                                               <*> switch    ( long "library"                                                    <> help "Compile as a library" )
                                               <*> strOption ( long "lib-name"    <> short 'n' <> value "name" <> metavar "NAME" <> help "Library name"    )
                                               -- TODO [PM] : implement reading versions
                                               <*> option    ( long "lib-version" <> value (Version [1] []) <> metavar "VERSION" <> help "Library version in X.Y.Z format" )
                                               <*> strOption ( long "root-path"                <> value "" <> hidden )
                                               <*> switch    ( long "global"  <> help "Compile to global library" )
                                             
                                               <*> switch ( long "dump-all"  <> hidden )
                                               <*> switch ( long "dump-ast"  <> hidden )
                                               <*> switch ( long "dump-va"   <> hidden )
                                               <*> switch ( long "dump-fp"   <> hidden )
                                               <*> switch ( long "dump-ssa"  <> hidden )
                                               <*> switch ( long "dump-hast" <> hidden )
                                               <*> switch ( long "dump-hsc"  <> hidden ) 
                            )

repoParser :: Opt.Parser Cmd.Command
repoParser = Cmd.Repo <$> subparser ( command "clean"   (Opt.info (pure Cmd.Doc)   (progDesc "remove object files"))
                                    )

parser :: Opt.Parser Cmd.Prog
parser = Cmd.Prog <$> subparser ( command "build"   (Opt.info buildParser      (progDesc "compile packages and dependencies"))
                               <> command "clean"   (Opt.info (pure Cmd.Doc)   (progDesc "remove object files"))
                               <> command "doc"     (Opt.info (pure Cmd.Doc)   (progDesc "run lunadoc on package sources"))
                               <> command "env"     (Opt.info (pure Cmd.Doc)   (progDesc "print Luna environment information"))
                               <> command "repo"    (Opt.info (pure Cmd.Doc)   (progDesc "manage Luna library repository"))
                               -- <> command "get"     (Opt.info (pure Cmd.Doc)   (progDesc "download and install packages and dependencies"))
                               -- <> command "install" (Opt.info (pure Cmd.Doc)   (progDesc "compile and install packages and dependencies"))
                               <> command "run"     (Opt.info (pure Cmd.Doc)   (progDesc "compile and run Luna program"))
                               <> command "version" (Opt.info versionParser    (progDesc "print Luna version"))
                                )
                  <*> switch    ( long "no-color" <> hidden <> help "disable color output" )
                  <*> optIntFlag Nothing 'v' 0 2 "verbose level [0-5], default 3"


opts :: Opt.ParserInfo Cmd.Prog
opts = Opt.info (parser <**> helper) (fullDesc <> header (Version.full False)) --idm 


helper :: Opt.Parser (a -> a)
helper = Opt.abortOption Opt.ShowHelpText $ (long "help" <> short 'h' <> help "show this help text")


main :: IO ()
main = Opt.execParser opts >>= run


run :: Cmd.Prog -> IO ()
run prog = case Cmd.cmd prog of
    Cmd.Version op -> putStrLn (Version.full $ Cmd.numeric op)
    Cmd.Build   op -> do rootLogger setIntLevel $ Cmd.verbose prog 
                         cfg <- Config.load
                         Build.run cfg op
