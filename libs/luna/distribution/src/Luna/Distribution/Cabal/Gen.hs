---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Distribution.Cabal.Gen where

import qualified Data.Text.Lazy as Text
import           Data.Version   (Version)

import           Flowbox.Prelude
import           Luna.Data.Source                (Source)
import qualified Luna.Data.Source                as Source
import           Luna.Distribution.Cabal.Config  (Config)
import qualified Luna.Distribution.Cabal.Config  as Config
import           Luna.Distribution.Cabal.Section (Section)
import qualified Luna.Distribution.Cabal.Section as Section
import qualified Luna.Syntax.Name.Path           as Name



getModuleName :: Source a -> String
getModuleName = Text.unpack . Text.intercalate "." . toList . view Source.modName


genLibrary ::  String -> Version -> [String] -> [String] -> [String] -> [String] -> [Source a] -> Config
genLibrary name version ghcOptions ccOptions includeDirs libs sources = genCommon sectionBase name version ghcOptions ccOptions includeDirs libs where
    sectionBase = Section.mkLibrary { Section.exposedModules = map getModuleName sources }


genExecutable :: String -> Version -> [String] -> [String] -> [String] -> [String] -> Config
genExecutable name version ghcOptions ccOptions includeDirs libs = genCommon sectionBase name version ghcOptions ccOptions includeDirs libs where
    sectionBase = Section.mkExecutable name


genCommon :: Section -> String -> Version -> [String] -> [String] -> [String] -> [String] -> Config
genCommon sectionBase name version ghcOptions ccOptions includeDirs libs = conf where
    section = sectionBase { Section.buildDepends = libs
                          , Section.ghcOptions   = ghcOptions
                          , Section.ccOptions    = ccOptions
                          , Section.includeDirs  = includeDirs
                          }
    conf = Config.addSection section
         $ Config.make name version
