---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Section where

import           Flowbox.Prelude                    
import           Flowbox.Luna.Data.HAST.Extension   (Extension)

import           Data.String.Utils                  (join)



data Section = Library    { exposedModules :: [String]
                          , hsSourceDirs   :: [String]
                          , ghcOptions     :: [String]
                          , extensions     :: [Extension]
                          , buildDepends   :: [String]
                          }
             | Executable { name           :: String
                          , mainIs         :: String
                          , hsSourceDirs   :: [String]
                          , ghcOptions     :: [String]
                          , extensions     :: [Extension]
                          , buildDepends   :: [String]
                          }
                deriving (Show)


mkExecutable :: String -> Section
mkExecutable name' = mkCommon $ Executable name' "Main.hs" 


mkLibrary :: Section
mkLibrary = mkCommon $ Library []


mkCommon :: ([String] -> [String] -> [Extension] -> [String] -> Section) -> Section
mkCommon s = s ["src"] ["-Wall"] [] ["base"]


defaultIndent :: String
defaultIndent = replicate 18 ' '


ident :: String
ident = replicate 4 ' '


genFields :: String -> [String] -> String
genFields name' vals = genField name' $ if null vals
    then ""
    else join (",\n" ++ defaultIndent) vals


genField :: String -> String -> String
genField name' val = ident 
                   ++ name' ++ ":" 
                   ++ replicate (18 - length name') ' ' 
                   ++ val ++ "\n"


genCode :: Section -> String
genCode s = genCodeSpecific s 
               ++ genFields "Hs-Source-Dirs"  (hsSourceDirs s)
               ++ genFields "GHC-Options"     (ghcOptions s)
               ++ genFields "Extensions"      (map show $ extensions s)
               ++ genFields "Build-Depends"   (buildDepends s)
   

genCodeSpecific :: Section -> String
genCodeSpecific s = case s of 
    Library {}    -> "Library\n"
               ++ genFields "Exposed-modules" (exposedModules s)
    Executable {} -> "Executable " ++ (name s) ++ "\n"
               ++ genField  "Main-Is"         (mainIs s)