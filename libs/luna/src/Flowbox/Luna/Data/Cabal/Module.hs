---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Module where

import qualified Data.List                       as List

import           Flowbox.Prelude                   
import qualified Flowbox.Luna.Data.Cabal.Section as Section
import           Flowbox.Luna.Data.Cabal.Section   (Section(Section))



data Module = Library    { sections :: [Section]
                              }
                 | Executable { name     :: String
                              , sections :: [Section] 
                              }


mkLibrary :: [String] -> String -> [String] -> [String] -> [String] -> Module
mkLibrary hsSourceDirs ghcOptions extensions buildDepends exposedModules = 
    Library [ Section "Hs-Source-Dirs" hsSourceDirs 
            , Section "GHC-Options" [ghcOptions]
            , Section "Extensions" extensions
            , Section "Build-Depends" buildDepends
            , Section "Exposed-modules" exposedModules
            ]


mkExecutable :: String -> [String] -> String -> [String] -> [String] -> String -> Module
mkExecutable name' hsSourceDirs ghcOptions extensions buildDepends mainIs = 
    Executable name' [ Section "Hs-Source-Dirs" hsSourceDirs 
                    , Section "GHC-Options" [ghcOptions]
                    , Section "Extensions" extensions
                    , Section "Build-Depends" buildDepends
                    , Section "Main-Is" [mainIs]
                    ]


generate :: Module -> String
generate m = r where 
    r = case m of 
        Library      _ -> "Library:\n"               ++ body
        Executable n _ -> "Executable " ++ n ++ "\n" ++ body
    body = List.concat $ map (Section.generate 4) $ sections m
