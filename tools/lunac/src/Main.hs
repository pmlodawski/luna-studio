---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Options.Applicative                   as Opt 
import           Options.Applicative                   ( (<>), Parser, ParserInfo, ParseError(ShowHelpText)
                                                       , argument, str, metavar, switch ,fullDesc, header
                                                       , long, help, strOption, value, hidden, subparser
                                                       , short, command, progDesc, command, execParser, abortOption)

import           Flowbox.Prelude 
import           Flowbox.Control.Applicative       
import qualified Flowbox.Lunac.Cmd                     as Cmd
import           Flowbox.Lunac.Cmd                     (Command, Prog)
import qualified Flowbox.Lunac.Version as Version
import           Flowbox.Options.Applicative           (optIntFlag)
import           Flowbox.System.Log.Logger 

                                                       

rootLogger :: Logger
rootLogger = getLogger "Flowbox.Lunac"


versionParser :: Parser Command
versionParser = Cmd.Version <$> (Cmd.VersionOptions <$> switch ( long "compiler" <> help "Print only the Luna compiler version" )
                                                    <*> switch ( long "library"  <> help "Print only the Luna library version" )
                                                    <*> switch ( long "numeric"  <> help "Print only numeric version" )
                                )

buildParser :: Parser Command
buildParser = Cmd.Build <$> ( Cmd.BuildOptions <$> strOption ( long "output"  <> short 'o' <> value "out"  <> metavar "OUTPUT"  <> help "output file name" )
                                               <*> optIntFlag Nothing 'O' 0 2 "optimisation level [0-2], default 2"
                                               <*> strOption ( hidden <> long "dump" <> value "" <> metavar "DUMP")
                            )

parser :: Parser Prog
parser = Cmd.Prog <$> subparser ( command "build"   (Opt.info buildParser      (progDesc "compile packages and dependencies"))
                               <> command "clean"   (Opt.info (pure Cmd.Doc)   (progDesc "remove object files"))
                               <> command "doc"     (Opt.info (pure Cmd.Doc)   (progDesc "run lunadoc on package sources"))
                               <> command "env"     (Opt.info (pure Cmd.Doc)   (progDesc "print Go environment information"))
                               <> command "get"     (Opt.info (pure Cmd.Doc)   (progDesc "download and install packages and dependencies"))
                               <> command "install" (Opt.info (pure Cmd.Doc)   (progDesc "compile and install packages and dependencies"))
                               <> command "run"     (Opt.info (pure Cmd.Doc)   (progDesc "compile and run Luna program"))
                               <> command "version" (Opt.info versionParser    (progDesc "print Luna version"))
                                )
                  <*> switch    ( long "no-color" <> hidden <> help "disable color output" )
                  <*> optIntFlag Nothing 'v' 0 2 "verbose level [0-5], default 3"


opts :: ParserInfo Prog
opts = Opt.info (parser <**> helper) (fullDesc <> header (Version.full False)) --idm 


helper :: Parser (a -> a)
helper = abortOption ShowHelpText $ (long "help" <> short 'h' <> help "show this help text")


main :: IO ()
main = execParser opts >>= run

run :: Prog -> IO ()
run prog = case Cmd.cmd prog of
    Cmd.Version op -> putStrLn (Version.full $ Cmd.numeric op)
    Cmd.Build   op -> print $ Cmd.optimisation op

----parser :: Parser CmdArgs
--parser =  subparser ( command "add"    (info addOptions  ( progDesc "Add a file to the repository" ))
--                   -- <> command "commit" (info commitOptions ( progDesc "Record changes to the repository" ))
--                    )
--    where addOptions    = Opt.flag' CmdArgs.Version (long "version" <> short 'V' <> hidden)
--          --commitOptions = Opt.flag' CmdArgs.Version (long "version" <> short 'V' <> hidden)

--    --Opt.flag' CmdArgs.Version    (long "version" <> short 'V' <> hidden)
--    --   <|> Opt.flag' CmdArgs.NumVersion (long "numeric-version"      <> hidden)
--    --   <|> Opt.flag' CmdArgs.Hello      (long "hello"                <> hidden)
--    --   <|> CmdArgs.Compilation
--    --       <$> many1     ( argument str ( metavar "INPUTS" ))
--    --       <*> switch    ( long "version" <> short 'V'                                      <> help "Print version information" )
--    --       <*> switch    ( long "numeric-version"                                           <> help "Print just the version number" )
--    --       <*> optIntFlag (Just "verbose") 'v' 2 3                                                  "Verbose level (level range is 0-5, default level is 3)"
--    --       <*> switch    ( long "no-color"                                                  <> help "Disable color output" )

--    --       <*> optIntFlag Nothing 'O' 0 2                                                           "Optimisation level (level range is 0-2, default level is 2)"
--    --       <*> strOption ( long "output"  <> short 'o' <> value "out"  <> metavar "OUTPUT"  <> help "Output folder" )
--    --       <*> many      ( strOption (       short 'l'                 <> metavar "LIBRARY" <> help "Library to link with."))
           
--    --       <*> switch    ( long "library"                                                   <> help "Compile as a library" )
--    --       <*> strOption ( long "lib-name"    <> short 'n' <> value "name" <> metavar "NAME"    <> help "Library name"    )
--    --       <*> option    ( long "lib-version" <> short 'n' <> value (Version [1,0,0] [])  <> metavar "VERSION" <> help "Library version in X.Y.Z format" )
--    --       <*> strOption ( long "root-path"  <> value "" <> hidden )
--    --       <*> switch    ( long "global"                                                    <> help "Compile to global library" )

--    --       <*> switch    ( long "dump-all"               <> hidden                                                      )
--    --       <*> switch    ( long "dump-ast"               <> hidden                                                      )
--    --       <*> switch    ( long "dump-va"                <> hidden                                                      )
--    --       <*> switch    ( long "dump-fp"                <> hidden                                                      )
--    --       <*> switch    ( long "dump-ssa"               <> hidden                                                      )
--    --       <*> switch    ( long "dump-hast"              <> hidden                                                      )
--    --       <*> switch    ( long "dump-hsc"               <> hidden                                                      )


--opts :: ParserInfo CmdArgs
--opts = Opt.info (helper <*> parser)
--              (Opt.fullDesc
--                  <> Opt.header (show_version)
--              )


--show_version :: String
--show_version = "Luna compiler, version " ++ Version.showVersion Config.version

--show_num_version :: String
--show_num_version = Version.showVersion Config.version


--main :: IO ()
--main = do
--    execParser opts >>= run


--run :: CmdArgs -> IO ()
--run cmd = do
--    case cmd of
--        CmdArgs.Version     {} -> putStrLn $ show_version
--        CmdArgs.NumVersion  {} -> putStrLn $ show_num_version
--        CmdArgs.Hello       {} -> putStrLn $ "Hello, my name is John le Box. Nice to meet you :)"
--        CmdArgs.Compilation {} -> do

--            rootLogger setIntLevel $ CmdArgs.verbose cmd

--            let diag = Diagnostics False
--                                 ( CmdArgs.dump_ast  cmd || CmdArgs.dump_all cmd )
--                                 ( CmdArgs.dump_va   cmd || CmdArgs.dump_all cmd )
--                                 ( CmdArgs.dump_fp   cmd || CmdArgs.dump_all cmd )
--                                 ( CmdArgs.dump_ssa  cmd || CmdArgs.dump_all cmd )
--                                 ( CmdArgs.dump_hast cmd || CmdArgs.dump_all cmd )
--                                 ( CmdArgs.dump_hsc  cmd || CmdArgs.dump_all cmd )

--                inputs = map UniPath.fromUnixString $ CmdArgs.inputs cmd


--            --Initializer.initializeIfNeeded cfg

--            --mapM_ (Builder.build cfg cmd diag) inputs
--            return ()
      

