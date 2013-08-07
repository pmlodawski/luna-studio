---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Cabal.Section (
    Section(..),
    empty,
    genCode,
)where

import Debug.Trace

import qualified Luna.Codegen.Hs.AST.Expr        as Expr
import           Luna.Codegen.Hs.AST.Expr          (Expr)
import           Data.String.Utils                 (join)
import qualified Luna.Codegen.Hs.Path            as Path
import qualified Luna.Codegen.Hs.AST.Function    as Function
import           Luna.Codegen.Hs.AST.Function      (Function)
import qualified Luna.Codegen.Hs.AST.Extension   as Extension
import           Luna.Codegen.Hs.AST.Extension     (Extension)


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
   