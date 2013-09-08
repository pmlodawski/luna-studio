---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.Cabal.Config (
    Config(..),
    empty,
    genCode,
    addSection
)where

import           Flowbox.Prelude                           
import           Debug.Trace                               
import           Data.String.Utils                         (join)
import qualified Flowbox.Luna.Passes.HSGen.AST.Expr      as Expr
import           Flowbox.Luna.Passes.HSGen.AST.Expr        (Expr)
import qualified Flowbox.Luna.Passes.HSGen.Path          as Path
import qualified Flowbox.Luna.Passes.HSGen.AST.Function  as Function
import           Flowbox.Luna.Passes.HSGen.AST.Function    (Function)
import qualified Flowbox.Luna.Passes.HSGen.Cabal.Section as Section
import           Flowbox.Luna.Passes.HSGen.Cabal.Section   (Section)


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
genField name' value = name' ++ ":" ++ replicate (18 - length name') ' ' ++ value ++ "\n"


genCode :: Config -> String
genCode conf =  genField "Name"          (name conf)
             ++ genField "Version"       (version conf)
             ++ genField "Cabal-Version" (cabalVersion conf)
             ++ genField "Build-Type"    (buildType conf)
             ++ "\n" ++  join "\n\n" (map Section.genCode $ sections conf)
   

addSection :: Section -> Config -> Config
addSection s conf = conf { sections = s:sections conf }