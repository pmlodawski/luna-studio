---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.Cabal.Section (
    Section(..),
    empty,
    genCode,
)where

import           Flowbox.Prelude                           

import qualified Flowbox.Luna.Passes.HSGen.AST.Expr      as Expr
import           Flowbox.Luna.Passes.HSGen.AST.Expr        (Expr)
import qualified Flowbox.Luna.Passes.HSGen.Path          as Path
import qualified Flowbox.Luna.Passes.HSGen.AST.Function  as Function
import           Flowbox.Luna.Passes.HSGen.AST.Function    (Function)
import qualified Flowbox.Luna.Passes.HSGen.AST.Extension as Extension
import           Flowbox.Luna.Passes.HSGen.AST.Extension   (Extension)

import           Debug.Trace                               
import           Data.String.Utils                         (join)


data SectionType = Library | Executable deriving(Show)


data Section = Section { cls            :: SectionType
                       , hsSourceDirs   :: [String]
                       , ghcOptions     :: [String]
                       , extensions     :: [Extension]
                       , exposedModules :: [String]
                       , buildDepends   :: [String]
                     } deriving (Show)


empty :: Section
empty = Section Library ["src"] ["-Wall"] [] [] []


defaultIndent :: String
defaultIndent = replicate 18 ' '

ident :: String
ident = replicate 4 ' '


genField :: String -> [String] -> String
genField name' values = if null values
	then ""
	else ident ++ name' ++ ":" ++ replicate (18 - length name') ' ' 
         ++ join (",\n" ++ defaultIndent) values ++ "\n"


genCode :: Section -> String
genCode s =  show (cls s) ++ "\n"
	      ++ genField "Hs-Source-Dirs"  (hsSourceDirs s)
          ++ genField "GHC-Options"     (ghcOptions s)
          ++ genField "Extensions"      (map show $ extensions s)
          ++ genField "Exposed-modules" (exposedModules s)
          ++ genField "Build-Depends"   (buildDepends s)
   