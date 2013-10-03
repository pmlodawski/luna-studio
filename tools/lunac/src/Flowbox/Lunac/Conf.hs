---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.Conf where

import           Flowbox.Prelude   



data Conf = Compilation { inputs    :: [String]
                        , output    :: String
                        , project   :: String
                        , verbose   :: Bool
                        , noColor   :: Bool
                        , dump_all  :: Bool
                        , dump_ast  :: Bool
                        , dump_va   :: Bool
                        , dump_ssa  :: Bool
                        , dump_hast :: Bool
                        , dump_hsc  :: Bool
                        }
          | Version
          deriving Show
