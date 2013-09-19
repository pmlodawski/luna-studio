---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.Cabal.CabalModule where

import qualified Data.List                         as List

import           Flowbox.Prelude                     
import qualified Flowbox.Luna.Passes.Cabal.Section as Section
import           Flowbox.Luna.Passes.Cabal.Section   (Section(Section))



data CabalModule = Library    { sections :: [Section]
                              }
                 | Executable { name     :: String
                              , sections :: [Section] 
                              }


mkLibrary :: [String] -> String -> [String] -> [String] -> [String] -> CabalModule
mkLibrary hsSourceDirs ghcOptions extensions buildDepends exposedModules = 
    Library [ Section "Hs-Source-Dirs" hsSourceDirs 
            , Section "GHC-Options" [ghcOptions]
            , Section "Extensions" extensions
            , Section "Build-Depends" buildDepends
            , Section "Exposed-modules" exposedModules
            ]


mkExecutable :: String -> [String] -> String -> [String] -> [String] -> String -> CabalModule
mkExecutable name' hsSourceDirs ghcOptions extensions buildDepends mainIs = 
    Executable name' [ Section "Hs-Source-Dirs" hsSourceDirs 
                    , Section "GHC-Options" [ghcOptions]
                    , Section "Extensions" extensions
                    , Section "Build-Depends" buildDepends
                    , Section "Main-Is" [mainIs]
                    ]


generate :: CabalModule -> String
generate m = r where 
    r = case m of 
        Library      _ -> "Library:\n"               ++ body
        Executable n _ -> "Executable " ++ n ++ "\n" ++ body
    body = List.concat $ map (Section.generate 4) $ sections m
