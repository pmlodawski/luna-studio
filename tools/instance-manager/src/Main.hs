---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

import           Options.Applicative (argument, command, command, fullDesc, help, hidden, long, metavar, prefs, progDesc, short, str, strOption, subparser, switch, value, (<>))
import qualified Options.Applicative as Opt

import qualified Flowbox.AWS.Region                      as Region
import           Flowbox.Control.Applicative
import qualified Flowbox.InstanceManager.Cmd             as Cmd
import qualified Flowbox.InstanceManager.Config          as Config
import qualified Flowbox.InstanceManager.InstanceManager as InstanceManager
import qualified Flowbox.InstanceManager.Version         as Version
import           Flowbox.Options.Applicative             (optIntFlag)
import           Flowbox.Prelude                         hiding (argument, op)
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


startParser :: Opt.Parser Cmd.Command
startParser = Cmd.Start <$> ( Cmd.StartOptions <$> strOption ( long "ami"     <> short 'a' <> value Config.defaultAmi     <> metavar "ami-id"       <> help ("Specify AMI to run, default is " ++ Config.defaultAmi))
                                               <*> strOption ( long "machine" <> short 'm' <> value Config.defaultMachine <> metavar "machine-type" <> help ("Specify machine type to run, default is " ++ Config.defaultMachine))
                                               <*> credOption
                          )


stopParser :: Opt.Parser Cmd.Command
stopParser = Cmd.Stop <$> ( Cmd.StopOptions <$> argument str ( metavar "ami-id" <> help "Specify instance instance-id")
                                            <*> switch       ( long "force"    <> short 'f' <> help "Force instance stop" )
                                            <*> credOption
                          )


getParser :: Opt.Parser Cmd.Command
getParser = Cmd.Get <$> ( Cmd.GetOptions <$> credOption )


versionParser :: Opt.Parser Cmd.Command
versionParser = Cmd.Version <$> (Cmd.VersionOptions <$> switch ( long "numeric"  <> help "print only numeric version" )
                                )


parser :: Opt.Parser Cmd.Prog
parser = Cmd.Prog <$> subparser ( command "start"   (Opt.info startParser   (progDesc "Start EC2 instance"))
                               <> command "stop"    (Opt.info stopParser    (progDesc "Stop EC2 instance"))
                               <> command "get"     (Opt.info getParser     (progDesc "Get EC2 instance ID and IP"))
                               <> command "version" (Opt.info versionParser (progDesc "Print instance-manager version"))
                                )
                  <*> strOption ( long "region" <> short 'r' <> value Config.defaultRegion <> metavar "region" <> help ("Specify AWS region, default is " ++ Config.defaultRegion))
                  <*> switch    ( long "no-color" <> hidden <> help "disable color output" )
                  <*> optIntFlag Nothing 'v' 3 2 "verbose level [0-5], default 3"
                  <**> helper


credOption :: Opt.Parser String
credOption = strOption ( long "cred" <> short 'c' <> value "aws.config" <> metavar "path" <> help "Path to a file with AWS credentials")


opts :: Opt.ParserInfo Cmd.Prog
opts = Opt.info parser (fullDesc <> Opt.progDesc "Flowbox AWS EC2 instance manager")


helper :: Opt.Parser (a -> a)
helper = Opt.abortOption Opt.ShowHelpText $ (long "help" <> short 'h' <> help "show this help text")


main :: IO ()
main = run =<< Opt.customExecParser
              (prefs Opt.showHelpOnError)
              opts


run :: Cmd.Prog -> IO ()
run prog = do
    rootLogger setIntLevel $ Cmd.verbose prog
    let region = Region.mk $ Cmd.region prog
    case Cmd.cmd prog of
      Cmd.Version op -> putStrLn $ Version.full (Cmd.numeric op)
      Cmd.Start   op -> InstanceManager.start region op
      Cmd.Get     op -> InstanceManager.get  region op
      Cmd.Stop    op -> InstanceManager.stop region op

