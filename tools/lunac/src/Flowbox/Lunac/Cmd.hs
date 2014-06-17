---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Lunac.Cmd where

import Data.Version (Version)

import Flowbox.Prelude



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
                              , libVersion   :: Version
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
                              }
              | ListOptions   { inputs :: [String]
                              --, installed :: Bool
                              , json   :: Bool
                              , simple :: Bool
                              , html   :: Bool
                              }
              | RepoOptions   {

                              }
             deriving Show
