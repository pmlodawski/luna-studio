---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.InstanceManager.Cmd where

import Flowbox.Prelude



data Prog    = Prog { cmd      :: Command
                    , region   :: String
                    , no_color :: Bool
                    , verbose  :: Int
                    }
             deriving Show


data Command = Start   Options
             | Stop    Options
             | Get     Options
             | Version Options
             deriving Show


data Options = StartOptions   { ami            :: String
                              , machine        :: String
                              , credentialPath :: String
                              }
             | StopOptions    { instanceID     :: String
                              , force          :: Bool
                              , credentialPath :: String
                              }
             | GetOptions     { credentialPath :: String
                              }
             | VersionOptions { numeric :: Bool
                              }
             deriving Show
