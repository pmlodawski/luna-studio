---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Luna.Console.Options where

import qualified Data.Version                as V
import           Options.Applicative         (argument, auto, command, command, fullDesc, help, hidden, long, metavar, option, prefs,
                                              progDesc, short, str, strOption, subparser, switch, value, (<>))
import qualified Options.Applicative         as Opt

--import qualified Flowbox.Config.Config       as Config
import           Flowbox.Control.Applicative
--FIXME[pm]: re-enable Flowbox.Distribution
--import qualified Flowbox.Distribution.Client.List as DistList
import           Flowbox.Options.Applicative (optIntFlag)
import           Flowbox.Prelude             hiding (argument, op, switch)
import           Flowbox.System.Log.Logger
--import qualified Luna.Build.Build            as Build
--import qualified Luna.Build.Version          as Version


data Prog    = Prog { cmd     :: Command
                    , noColor :: Bool
                    , verbose :: Int
                    }
             deriving Show

data Command = Hello
             | Build Options
             | Clean
             | Doc
             | Env
             | Get
             | Install
             | Run
             | Version Options
             | Repo { c :: Command }
             | List Options
             deriving Show


data Options = VersionOptions { compiler :: Bool
                              , library  :: Bool
                              , numeric  :: Bool
                              }
             | BuildOptions   { input        :: String
                              , output       :: String
                              , optimisation :: Int
                              , link         :: [String]

                              , library      :: Bool
                              , libName      :: String
                              , libVersion   :: V.Version
                              , rootPath     :: String
                              , global       :: Bool

                              , buildDir     :: String
                              , ddebug       :: Bool

                              , dump_all     :: Bool
                              , dump_ast     :: Bool
                              , dump_aa      :: Bool
                              , dump_ssa     :: Bool
                              , dump_hash    :: Bool
                              , dump_hast    :: Bool
                              , dump_hsc     :: Bool

                              , noStdlib     :: Bool
                              }
              | ListOptions   { inputs :: [String]
                              --, installed :: Bool
                              , json   :: Bool
                              , simple :: Bool
                              , html   :: Bool
                              }
              | RepoOptions
             deriving Show



rootLogger :: Logger
rootLogger = getLogger ""


versionParser :: Opt.Parser Command
versionParser = Version <$> (VersionOptions <$> switch ( long "compiler" <> help "print only the Luna compiler version" )
                                            <*> switch ( long "library"  <> help "print only the Luna library version" )
                                            <*> switch ( long "numeric"  <> help "print only numeric version" )
                            )

buildParser :: Opt.Parser Command
buildParser = Build <$> ( BuildOptions <$> argument str ( metavar "INPUTS" )
                                       <*> strOption ( long "output"  <> short 'o' <> value "out"  <> metavar "OUTPUT"  <> help "output file name" )
                                       <*> optIntFlag Nothing 'O' 0 2 "optimisation level [0-2], default 2"
                                       <*> many      ( strOption ( short 'l' <> metavar "LIBRARY" <> help "library to link with"))

                                       <*> switch    ( long "library"                                                    <> help "compile as a library" )
                                       <*> strOption ( long "lib-name"    <> short 'n' <> value "name" <> metavar "NAME" <> help "library name"    )
                                       -- TODO [PM] : implement reading versions
                                       <*> option auto ( long "lib-version" <> value (V.Version [1] []) <> metavar "VERSION" <> help "library version in X.Y.Z format" )
                                       <*> strOption ( long "root-path"                <> value "" <> hidden )
                                       <*> switch    ( long "global"  <> help "compile to global library" )
                                       <*> strOption ( long "build-dir"                <> value "" <> hidden )

                                       <*> switch ( long "DDEBUG"    <> hidden )
                                       <*> switch ( long "dump-all"  <> hidden )
                                       <*> switch ( long "dump-ast"  <> hidden )
                                       <*> switch ( long "dump-aa"   <> hidden )
                                       <*> switch ( long "dump-ssa"  <> hidden )
                                       <*> switch ( long "dump-hash" <> hidden )
                                       <*> switch ( long "dump-hast" <> hidden )
                                       <*> switch ( long "dump-hsc"  <> hidden )
                                       <*> switch ( long "no-stdlib" <> hidden )
                        )


listParser :: Opt.Parser Command
listParser = List <$> ( ListOptions <$> many (argument str ( metavar "PATTERNS" ))
                                    -- <*> switch    ( long "installed"     <> help "only list installed packages" )
                                    <*> switch    ( long "json"          <> help "list packages using JSON data serialization format" )
                                    <*> switch    ( long "simple"        <> help "list packages using simple output form" )
                                    <*> switch    ( long "html"          <> help "list packages with description in HTML format" )
                      )

repoParser :: Opt.Parser Command
repoParser = Repo <$> subparser (   command "install"   (Opt.info (pure Doc)   (progDesc "compile and install packages and dependencies"))
                                  <> command "info"      (Opt.info (pure Doc)   (progDesc "display detailed information about particular package"))
                                  <> command "list"      (Opt.info listParser       (progDesc "list packages matching a search string"))
                                  <> command "uninstall" (Opt.info (pure Doc)   (progDesc "uninstall selected packages"))
                                  <> command "upload"    (Opt.info (pure Doc)   (progDesc "upload packages to remote repository"))
                                )
                  <**> helper

parser :: Opt.Parser Prog
parser = Prog <$> subparser (    command "build"   (Opt.info buildParser      (progDesc "compile packages and dependencies"))
                              <> command "clean"   (Opt.info (pure Doc)   (progDesc "remove object files"))
                              <> command "doc"     (Opt.info (pure Doc)   (progDesc "run lunadoc on package sources"))
                              <> command "env"     (Opt.info (pure Doc)   (progDesc "print Luna environment information"))
                              -- <> command "get"     (Opt.info (pure Doc)   (progDesc "download and install packages and dependencies"))
                              <> command "install" (Opt.info (pure Doc)   (progDesc "compile and install packages and dependencies"))
                              <> command "repo"    (Opt.info repoParser       (progDesc "manage Luna library repository"))
                              <> command "run"     (Opt.info (pure Doc)   (progDesc "compile and run Luna program"))
                              <> command "version" (Opt.info versionParser    (progDesc "print Luna version"))
                            )
              <*> switch    ( long "no-color" <> hidden <> help "disable color output" )
              <*> optIntFlag Nothing 'v' 3 2 "verbose level [0-5], default 3"
              <**> helper


opts :: Opt.ParserInfo Prog
opts = Opt.info parser (fullDesc <> Opt.progDesc "Luna compiler & package manager") --idm


helper :: Opt.Parser (a -> a)
helper = Opt.abortOption Opt.ShowHelpText (long "help" <> short 'h' <> help "show this help text")


--main :: IO ()
--main = run =<< Opt.customExecParser
--              (prefs Opt.showHelpOnError)
--              opts


--run :: Prog -> IO ()
--run prog = case cmd prog of
--    Version op -> putStrLn $ Version.full (numeric op) (compiler op) (library op)
--    Build   op -> do rootLogger setIntLevel $ verbose prog
--                         cfg <- Config.load
--                         Build.run cfg op
--    --FIXME[pm]: re-enable Repo
--    --Repo  scmd -> case scmd of
--    --                  List op -> list (simple op) (inputs op)
--    --                                 where list = if (json op) then DistList.listJSON (html op) else DistList.list
--    _              -> putStrLn "Sorry, the command is not implemented yet."

