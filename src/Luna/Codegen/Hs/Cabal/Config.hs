---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.Cabal.Config (
    Config(..),
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
import qualified Luna.Codegen.Hs.Cabal.Section   as Section
import           Luna.Codegen.Hs.Cabal.Section     (Section)


data Config = Config { name         :: String,
                       version      :: String,
                       cabalVersion :: String,
                       buildType    :: String,
                       sections     :: [Section]
                     } deriving (Show)


empty :: Config
empty = Config "" "1.0" ">= 1.8" "Simple" []

defaultIndent :: String
defaultIndent = replicate 18 ' '


genField :: String -> String -> String
genField name' value = name' ++ ":" ++ replicate (18 - length name') ' ' ++ value


genCode :: Config -> String
genCode conf = genField "Name" (name conf)
   