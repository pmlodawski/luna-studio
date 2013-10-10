---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.CodeGen.Cabal.Gen where

import qualified Data.List                       as List

import           Flowbox.Prelude                   
import qualified Flowbox.Luna.Data.Cabal.Config  as Config
import           Flowbox.Luna.Data.Cabal.Config    (Config)
import qualified Flowbox.Luna.Data.Cabal.Section as Section
import           Flowbox.Luna.Data.Cabal.Section   (Section)
import qualified Flowbox.Luna.Data.Source        as Source
import           Flowbox.Luna.Data.Source          (Source)



getModuleName :: Source -> String
getModuleName source = List.intercalate "." $ Source.path source


genLibrary :: [Source] -> String -> [String] -> Config
genLibrary sources =
    genCommon (Section.mkLibrary { Section.exposedModules = map getModuleName sources })


genExecutable :: String -> [String] -> Config
genExecutable name = 
    genCommon (Section.mkExecutable name) name


genCommon :: Section -> String -> [String] -> Config
genCommon section_base name libs = conf where
    section = section_base { Section.buildDepends = "pretty-show"
                                                  : "random"
                                                  : "base"
                                                  : "OneTuple"
                                                  : "template-haskell"
                                                  : "flowboxM-stdlib-io"
                                                  : libs
                           }
    conf = Config.addSection section 
         $ Config.make name
