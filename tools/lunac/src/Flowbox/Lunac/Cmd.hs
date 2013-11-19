---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.Cmd where

import           Flowbox.Prelude 


data Prog    = Prog { cmd      :: Command 
                    , no_color :: Bool
                    , verbose  :: Int
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
             deriving Show

data Options = VersionOptions { compiler :: Bool, library :: Bool, numeric :: Bool }
             | BuildOptions   { output :: String, optimisation :: Int, dump :: String }
             deriving Show