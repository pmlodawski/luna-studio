---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Lunac.Conf where

import           Flowbox.Prelude   



data Conf = Compilation { inputs   :: [String]
                        , verbose  :: Bool
                        , debug    :: Bool
                        , noColor  :: Bool
                        , dump_ast :: Bool
                        }
          | Version
          deriving Show
