---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.HAST.Module (
        module Luna.Data.HAST.Module,
        module Luna.Data.HAST.Expr
)where

import Flowbox.Prelude
import Luna.Data.HAST.Expr
import Luna.Data.HAST.Extension (Extension)
import           Data.Text.Lazy (Text)


empty :: Expr
empty = Module "Unnamed" [] [] [] []

mk :: Text -> [Text] -> Expr
mk name path' = Module name path' [] [] []

addImport :: [Text] -> Expr -> Expr
addImport path' mod' = mod' { _imports = Import False path' Nothing : _imports mod' }

addExt :: Extension -> Expr -> Expr
addExt ext' mod' = mod' { _ext = ext' : _ext mod' }
