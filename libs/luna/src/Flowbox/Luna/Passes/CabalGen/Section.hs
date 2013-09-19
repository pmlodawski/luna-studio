---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CabalGen.Section where

import qualified Data.List       as List
import           Flowbox.Prelude   



data Section = Section { name  :: String
                       , items :: [String]  
                       }


generate :: Section -> String
generate (Section n i) = spaces 4 ++ n ++ ":\n" ++ body ++ "\n" where
    body = List.concat $ List.intersperse ("," ++ spaces 8 ++ "\n") i


spaces :: Int -> String
spaces num = List.replicate num ' ' 
