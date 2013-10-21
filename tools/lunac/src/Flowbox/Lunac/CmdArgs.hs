---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.CmdArgs where

import           Flowbox.Prelude                   
import           Flowbox.Luna.Data.Cabal.Version   (Version)



data CmdArgs = Compilation { inputs          :: [String]
                           
                           , version_tmp     :: Bool -- used only to generate help msg
                           , version_num_tmp :: Bool -- used only to generate help msg
                           , verbose         :: Int
                           , noColor         :: Bool

                           , optimisation    :: Int
                           , output          :: String
                           , link            :: [String]

                           , library         :: Bool
                           , libName         :: String
                           , libVersion      :: Version
                           , rootPath        :: String
                           , global          :: Bool
                                              
                           , dump_all        :: Bool
                           , dump_ast        :: Bool
                           , dump_va         :: Bool
                           , dump_fp         :: Bool
                           , dump_ssa        :: Bool
                           , dump_hast       :: Bool
                           , dump_hsc        :: Bool
                           }
             | Version
             | NumVersion
             | Hello
             deriving Show




