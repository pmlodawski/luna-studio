---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.Cabal.Section where

import           Flowbox.Prelude                    
import           Flowbox.Luna.Data.HAST.Extension   (Extension)

import           Debug.Trace                        
import           Data.String.Utils                  (join)


data SectionType = Library | Executable deriving(Show)


data Section = Section { cls            :: SectionType
                       , hsSourceDirs   :: [String]
                       , mainIs         :: String
                       , ghcOptions     :: [String]
                       , extensions     :: [Extension]
                       , exposedModules :: [String]
                       , buildDepends   :: [String]
                       } deriving (Show)


empty :: SectionType -> Section
empty t = Section t ["src"] "Main.hs" ["-Wall"] [] [] []


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
genCode s =  show (cls s) ++ "\n"
          ++ genFields "Hs-Source-Dirs"  (hsSourceDirs s)
          ++ genField  "Main-Is"         (mainIs s)
          ++ genFields "GHC-Options"     (ghcOptions s)
          ++ genFields "Extensions"      (map show $ extensions s)
          ++ genFields "Exposed-modules" (exposedModules s)
          ++ genFields "Build-Depends"   (buildDepends s)
   