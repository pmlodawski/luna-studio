---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.DEP.Pass.CodeGen.Cabal.Gen where

import qualified Data.List    as List
import           Data.Version (Version)

import           Flowbox.Prelude
import           Luna.DEP.Data.Source                (Source)
import qualified Luna.DEP.Data.Source                as Source
import           Luna.DEP.Distribution.Cabal.Config  (Config)
import qualified Luna.DEP.Distribution.Cabal.Config  as Config
import           Luna.DEP.Distribution.Cabal.Section (Section)
import qualified Luna.DEP.Distribution.Cabal.Section as Section



getModuleName :: Source -> String
getModuleName source = List.intercalate "." $ source ^. Source.path


genLibrary :: String -> Version -> [String] -> [String] -> [String] -> [Source] -> Config
genLibrary name version ghcOptions ccOptions libs sources = genCommon sectionBase name version ghcOptions ccOptions libs where
    sectionBase = Section.mkLibrary { Section.exposedModules = map getModuleName sources }


genExecutable :: String -> Version -> [String] -> [String] -> [String] -> Config
genExecutable name version ghcOptions ccOptions libs = genCommon sectionBase name version ghcOptions ccOptions libs where
    sectionBase = Section.mkExecutable name


genCommon :: Section -> String -> Version -> [String] -> [String] -> [String] -> Config
genCommon sectionBase name version ghcOptions ccOptions libs = conf where
    section = sectionBase { Section.buildDepends = libs
                          , Section.ghcOptions   = ghcOptions
                          , Section.ccOptions    = ccOptions
                          }
    conf = Config.addSection section
         $ Config.make name version
