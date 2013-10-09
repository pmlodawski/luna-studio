---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.CmdArgs where

import           Flowbox.Prelude   



data CmdArgs = Compilation { inputs    :: [String]
                           , link      :: [String]
                           , output    :: String
                           , name      :: String
                           , rootPath  :: String
                           
                           , global    :: Bool
                           , library   :: Bool
                           
                           , verbose   :: Bool
                           , noColor   :: Bool
                           
                           , dump_all  :: Bool
                           , dump_ast  :: Bool
                           , dump_va   :: Bool
                           , dump_fp   :: Bool
                           , dump_ssa  :: Bool
                           , dump_hast :: Bool
                           , dump_hsc  :: Bool
                           }
             | Version
             deriving Show
