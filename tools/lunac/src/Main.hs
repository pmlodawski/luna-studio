---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import           Data.Version        (Version (Version))
import           Options.Applicative (argument, prefs, command, command, fullDesc, help, hidden, long, metavar, option, progDesc, short, str, strOption, subparser, switch, value, (<>))
import qualified Options.Applicative as Opt

import qualified Flowbox.Config.Config            as Config
import           Flowbox.Control.Applicative
import qualified Flowbox.Distribution.Client.List as DistList
import qualified Flowbox.Lunac.Build              as Build
import qualified Flowbox.Lunac.Cmd                as Cmd
import qualified Flowbox.Lunac.Version            as Version
import           Flowbox.Options.Applicative      (optIntFlag)
import           Flowbox.Prelude                  hiding (argument, op)
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


versionParser :: Opt.Parser Cmd.Command
versionParser = Cmd.Version <$> (Cmd.VersionOptions <$> switch ( long "compiler" <> help "print only the Luna compiler version" )
                                                    <*> switch ( long "library"  <> help "print only the Luna library version" )
                                                    <*> switch ( long "numeric"  <> help "print only numeric version" )
                                )

buildParser :: Opt.Parser Cmd.Command
buildParser = Cmd.Build <$> ( Cmd.BuildOptions <$> argument str ( metavar "INPUTS" )
                                               <*> strOption ( long "output"  <> short 'o' <> value "out"  <> metavar "OUTPUT"  <> help "output file name" )
                                               <*> optIntFlag Nothing 'O' 0 2 "optimisation level [0-2], default 2"
                                               <*> many      ( strOption ( short 'l' <> metavar "LIBRARY" <> help "library to link with"))

                                               <*> switch    ( long "library"                                                    <> help "compile as a library" )
                                               <*> strOption ( long "lib-name"    <> short 'n' <> value "name" <> metavar "NAME" <> help "library name"    )
                                               -- TODO [PM] : implement reading versions
                                               <*> option    ( long "lib-version" <> value (Version [1] []) <> metavar "VERSION" <> help "library version in X.Y.Z format" )
                                               <*> strOption ( long "root-path"                <> value "" <> hidden )
                                               <*> switch    ( long "global"  <> help "compile to global library" )

                                               <*> switch ( long "dump-all"  <> hidden )
                                               <*> switch ( long "dump-ast"  <> hidden )
                                               <*> switch ( long "dump-va"   <> hidden )
                                               <*> switch ( long "dump-fp"   <> hidden )
                                               <*> switch ( long "dump-ssa"  <> hidden )
                                               <*> switch ( long "dump-hast" <> hidden )
                                               <*> switch ( long "dump-hsc"  <> hidden )
                            )

listParser :: Opt.Parser Cmd.Command
listParser = Cmd.List <$> ( Cmd.ListOptions <$> many (argument str ( metavar "PATTERNS" ))
                                            -- <*> switch    ( long "installed"     <> help "only list installed packages" )
                                            <*> switch    ( long "json"          <> help "list packages using JSON data serialization format" )
                                            <*> switch    ( long "simple"        <> help "list packages using simple output form" )
                          )

repoParser :: Opt.Parser Cmd.Command
repoParser = Cmd.Repo <$> subparser ( command "install"   (Opt.info (pure Cmd.Doc)   (progDesc "compile and install packages and dependencies"))
                                   <> command "info"      (Opt.info (pure Cmd.Doc)   (progDesc "display detailed information about particular package"))
                                   <> command "list"      (Opt.info listParser       (progDesc "list packages matching a search string"))
                                   <> command "uninstall" (Opt.info (pure Cmd.Doc)   (progDesc "uninstall selected packages"))
                                   <> command "upload"    (Opt.info (pure Cmd.Doc)   (progDesc "upload packages to remote repository"))
                                    )
                      <**> helper

parser :: Opt.Parser Cmd.Prog
parser = Cmd.Prog <$> subparser ( command "build"   (Opt.info buildParser      (progDesc "compile packages and dependencies"))
                               <> command "clean"   (Opt.info (pure Cmd.Doc)   (progDesc "remove object files"))
                               <> command "doc"     (Opt.info (pure Cmd.Doc)   (progDesc "run lunadoc on package sources"))
                               <> command "env"     (Opt.info (pure Cmd.Doc)   (progDesc "print Luna environment information"))
                               -- <> command "get"     (Opt.info (pure Cmd.Doc)   (progDesc "download and install packages and dependencies"))
                               <> command "install" (Opt.info (pure Cmd.Doc)   (progDesc "compile and install packages and dependencies"))
                               <> command "repo"    (Opt.info repoParser       (progDesc "manage Luna library repository"))
                               <> command "run"     (Opt.info (pure Cmd.Doc)   (progDesc "compile and run Luna program"))
                               <> command "version" (Opt.info versionParser    (progDesc "print Luna version"))
                                )
                  <*> switch    ( long "no-color" <> hidden <> help "disable color output" )
                  <*> optIntFlag Nothing 'v' 3 2 "verbose level [0-5], default 3"
                  <**> helper


opts :: Opt.ParserInfo Cmd.Prog
opts = Opt.info parser (fullDesc <> Opt.progDesc "Luna compiler & package manager") --idm


helper :: Opt.Parser (a -> a)
helper = Opt.abortOption Opt.ShowHelpText $ (long "help" <> short 'h' <> help "show this help text")


main :: IO ()
main = run =<< Opt.customExecParser
              (prefs Opt.showHelpOnError)
              opts


run :: Cmd.Prog -> IO ()
run prog = case Cmd.cmd prog of
    Cmd.Version op -> putStrLn $ Version.full (Cmd.numeric op) (Cmd.compiler op) (Cmd.library op)
    Cmd.Build   op -> do rootLogger setIntLevel $ Cmd.verbose prog
                         cfg <- Config.load
                         Build.run cfg op
    Cmd.Repo  scmd -> case scmd of
                      Cmd.List op -> list (Cmd.simple op) (Cmd.inputs op)
                                     where list = if (Cmd.json op) then DistList.listJSON else DistList.list
    _              -> putStrLn "Sorry, the command is not implemented yet."

