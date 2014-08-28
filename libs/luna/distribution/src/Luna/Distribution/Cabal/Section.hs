---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Distribution.Cabal.Section where

import Flowbox.Prelude
import Luna.Data.HAST.Extension (Extension)

import Data.String.Utils (join)



data Section = Library    { exposedModules :: [String]
                          , ghcOptions     :: [String]
                          , ccOptions      :: [String]
                          , buildDepends   :: [String]
                          , extensions     :: [Extension]
                          , hsSourceDirs   :: [String]
                          }
             | Executable { name         :: String
                          , mainIs       :: String
                          , ghcOptions   :: [String]
                          , ccOptions    :: [String]
                          , buildDepends :: [String]
                          , extensions   :: [Extension]
                          , hsSourceDirs :: [String]
                          }
                deriving (Show)


mkExecutable :: String -> Section
mkExecutable name' = mkCommon $ Executable name' "Main.hs"


mkLibrary :: Section
mkLibrary = mkCommon $ Library []


mkCommon :: ([String] -> [String] -> [String] -> [Extension] -> [String] -> Section) -> Section
mkCommon s = s [] [] [] [] ["src"]


defaultIndent :: String
defaultIndent = replicate 18 ' '


ident :: String
ident = replicate 4 ' '


genFields :: String -> [String] -> String
genFields name' vals = genField name' $ if null vals
    then ""
    else join (",\n" ++ defaultIndent) vals

genArgs :: String -> [String] -> String
genArgs name' vals = genField name' $ if null vals
    then ""
    else join " " vals


genField :: String -> String -> String
genField name' val = ident
                   ++ name' ++ ":"
                   ++ replicate (18 - length name') ' '
                   ++ val ++ "\n"


genCode :: Section -> String
genCode s = genCodeSpecific s
               ++ genFields "Hs-Source-Dirs"  (hsSourceDirs s)
               ++ genArgs   "GHC-Options"     (ghcOptions s)
               ++ genArgs   "CC-Options"      (ccOptions s)
               ++ genFields "Extensions"      (map show $ extensions s)
               ++ genFields "Build-Depends"   (buildDepends s)


genCodeSpecific :: Section -> String
genCodeSpecific s = case s of
    Library {}    -> "Library\n"
               ++ genFields "Exposed-modules" (exposedModules s)
    Executable {} -> "Executable " ++ (name s) ++ "\n"
               ++ genField  "Main-Is"         (mainIs s)


