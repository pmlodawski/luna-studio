---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.AST.AST (
    module Flowbox.Luna.Data.AST.AST,
    module Flowbox.Luna.Data.AST.Common
)where

import Flowbox.Luna.Data.AST.Common
import Flowbox.Luna.Data.AST.Expr   (Expr)
import Flowbox.Luna.Data.AST.Lit    (Lit)
import Flowbox.Luna.Data.AST.Module (Module)
import Flowbox.Luna.Data.AST.Pat    (Pat)
import Flowbox.Luna.Data.AST.Type   (Type)
import Flowbox.Prelude


data AST = Module Module
         | Expr Expr
         | Lit Lit
         | Pat Pat
         | Type Type
         deriving (Show)
