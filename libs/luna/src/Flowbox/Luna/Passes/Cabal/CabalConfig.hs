---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.CabalConfig where

import qualified Data.List                             as List

import           Flowbox.Prelude                         
import qualified Flowbox.Luna.Passes.Cabal.CabalModule as CabalModule
import           Flowbox.Luna.Passes.Cabal.CabalModule   (CabalModule)
import qualified Flowbox.Luna.Passes.Cabal.Section     as Section
import           Flowbox.Luna.Passes.Cabal.Section       (Section(Section))



data CabalConfig = CabalConfig { sections     :: [Section]
                               , cabalModules :: [CabalModule]
                               }


make :: String -> String -> String -> String -> [CabalModule] -> CabalConfig
make name version cabalVersion buildType modules = 
    CabalConfig [ Section "Name" [name]
                , Section "Version" [version]
                , Section "Cabal-Version" [cabalVersion]
                , Section "Build-Type" [buildType]
                ] modules


generate :: CabalConfig -> String
generate config = cabalHeader ++ "\n\n\n" ++ cabalBody where
    cabalHeader = List.concat $ map (Section.generate 0) $ sections config
    cabalBody   = List.concat $ List.intersperse "\n" $ map CabalModule.generate $ cabalModules config

