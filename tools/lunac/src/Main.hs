---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Main where

import           Data.Version        (Version (Version))
import           Options.Applicative (argument, command, command, fullDesc, help, hidden, long, metavar, option, prefs, progDesc, short, str, strOption, subparser, switch, value, (<>))
import qualified Options.Applicative as Opt

import qualified Flowbox.Config.Config       as Config
import           Flowbox.Control.Applicative
--FIXME[pm]: re-enable Flowbox.Distribution
--import qualified Flowbox.Distribution.Client.List as DistList
import           Flowbox.Options.Applicative (optIntFlag)
import           Flowbox.Prelude             hiding (argument, op)
import           Flowbox.System.Log.Logger
--import qualified Luna.Build.Build            as Build
--import qualified Luna.Build.Cmd              as Cmd
--import qualified Luna.Build.Version          as Version

import qualified Luna.Console.Options as Opt



main :: IO ()
main = run =<< Opt.customExecParser
              (prefs Opt.showHelpOnError)
              opts


run :: Opt.Prog -> IO ()
run prog = case Opt.cmd prog of
    Opt.Version op -> putStrLn $ Version.full (Opt.numeric op) (Opt.compiler op) (Opt.library op)
    Opt.Build   op -> do rootLogger setIntLevel $ Opt.verbose prog
                         cfg <- Config.load
                         Build.run cfg op
    --FIXME[pm]: re-enable Opt.Repo
    --Opt.Repo  scmd -> case scmd of
    --                  Opt.List op -> list (Opt.simple op) (Opt.inputs op)
    --                                 where list = if (Opt.json op) then DistList.listJSON (Opt.html op) else DistList.list
    _              -> putStrLn "Sorry, the command is not implemented yet."

