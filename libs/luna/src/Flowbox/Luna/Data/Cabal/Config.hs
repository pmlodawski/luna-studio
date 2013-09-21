---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Config where

import qualified Data.List                       as List

import           Flowbox.Prelude                   
import qualified Flowbox.Luna.Data.Cabal.Module  as Module
import           Flowbox.Luna.Data.Cabal.Module    (Module)
import qualified Flowbox.Luna.Data.Cabal.Section as Section
import           Flowbox.Luna.Data.Cabal.Section   (Section(Section))



data Config = Config { sections     :: [Section]
                               , cabalModules :: [Module]
                               }


make :: String -> String -> String -> String -> [Module] -> Config
make name version cabalVersion buildType modules = 
    Config [ Section "Name" [name]
                , Section "Version" [version]
                , Section "Cabal-Version" [cabalVersion]
                , Section "Build-Type" [buildType]
                ] modules


generate :: Config -> String
generate config = cabalHeader ++ "\n\n\n" ++ cabalBody where
    cabalHeader = List.concat $ map (Section.generate 0) $ sections config
    cabalBody   = List.concat $ List.intersperse "\n" $ map Module.generate $ cabalModules config

