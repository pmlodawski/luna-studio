---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Pass.Data.HAST.Module (
        module Flowbox.Luna.Data.HAST.Module,
        module Flowbox.Luna.Data.HAST.Expr
)where

import Flowbox.Luna.Data.HAST.Expr
import Flowbox.Luna.Data.HAST.Extension (Extension)
import Flowbox.Prelude


empty :: Expr
empty = Module [] [] [] []

mk :: [String] -> Expr
mk path' = Module path' [] [] []

addImport :: [String] -> Expr -> Expr
addImport path' mod' = mod' { imports = (Import False path' Nothing) : imports mod' }

addExt :: Extension -> Expr -> Expr
addExt ext' mod' = mod' { ext = ext':ext mod' }
