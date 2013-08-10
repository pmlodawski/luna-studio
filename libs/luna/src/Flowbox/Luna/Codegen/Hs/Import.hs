---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.Import (
    Import(..),
    noItems,
    qualified,
    simple,
    regular,
    common,
    genCode
)where

import qualified Flowbox.Luna.Codegen.Hs.Path as Path
import           Flowbox.Luna.Codegen.Hs.Path   (Path(..))


data Import = Regular   {path :: Path, item :: String} 
            | Qualified {path :: Path}
            deriving (Show, Ord, Eq)


noItems :: String
noItems = ""


qualified :: Path -> Import
qualified path' = Qualified path'


simple :: Path -> Import
simple path' = Regular path' noItems


regular :: Path -> String -> Import
regular path' item' = Regular path' item'


common :: String -> Import
common name = simple $ Path ["Common'", Path.mkClassName name]


genCode :: Import -> String
genCode imp = "import " ++ body where
    paths = Path.toModulePath (path imp) 
    src = Path.toString paths
    body = case imp of
        Regular _ item' -> src ++ els where
                               els = if item' == ""
                                   then ""
                                   else " (" ++ item' ++ ")"
        Qualified _     -> "qualified " ++ src -- ++ " as " ++ last paths