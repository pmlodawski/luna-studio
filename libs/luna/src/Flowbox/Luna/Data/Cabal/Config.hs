---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Config where

import           Flowbox.Prelude                   
import qualified Flowbox.Luna.Data.Cabal.Section as Section
import           Flowbox.Luna.Data.Cabal.Section   (Section)
import           Data.String.Utils                 (join)


data Config = Config { name         :: String
                     , version      :: String
                     , cabalVersion :: String
                     , buildType    :: String
                     , sections     :: [Section]
                     } deriving (Show)


make :: String -> Config
make name' = Config name' "1.0" ">= 1.8" "Simple" []


defaultIndent :: String
defaultIndent = replicate 18 ' '


genField :: String -> String -> String
genField name' value = name' ++ ":" ++ replicate (18 - length name') ' ' ++ value ++ "\n"


genCode :: Config -> String
genCode conf =  genField "Name"          (name conf)
             ++ genField "Version"       (version conf)
             ++ genField "Cabal-Version" (cabalVersion conf)
             ++ genField "Build-Type"    (buildType conf)
             ++ "\n" ++  join "\n\n" (map Section.genCode $ sections conf)
   

addSection :: Section -> Config -> Config
addSection s conf = conf { sections = s:sections conf }