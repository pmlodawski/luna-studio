---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Luna.Target.HS.AST.Module (
        module Luna.Target.HS.AST.Module,
        module Luna.Target.HS.AST.Expr
)where

import Flowbox.Prelude
import Luna.Target.HS.AST.Expr
import Luna.Target.HS.AST.Extension (Extension)
import           Data.Text.Lazy (Text)


empty :: Expr
empty = Module "Unnamed" [] [] [] []

mk :: Text -> [Text] -> Expr
mk name path' = Module name path' [] [] []

addImport :: Expr -> Expr -> Expr
addImport imp mod' = mod' { _imports = imp : _imports mod' }

addExt :: Extension -> Expr -> Expr
addExt ext' mod' = mod' { _ext = ext' : _ext mod' }
