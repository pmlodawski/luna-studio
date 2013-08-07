---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Cabal.Section (
    Section(..),
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


data Section = Library { hsSourceDirs   :: [String]
                       , ghcOptions     :: [String]
                       , extensions     :: [Extension]
                       , exposedModules :: [String]
                       , buildDepends   :: [String]
                     } deriving (Show)


defaultIndent :: String
defaultIndent = replicate 18 ' '

genField :: String -> [String] -> String
genField name' values =  name' ++ ":" ++ replicate (18 - length name') ' ' 
                      ++ join (",\n" ++ defaultIndent) values


genCode :: Section -> String
genCode s = genField "Hs-Source-Dirs" (hsSourceDirs s)
   