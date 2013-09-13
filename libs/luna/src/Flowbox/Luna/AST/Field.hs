---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.AST.Field where

import           Flowbox.Prelude         
import qualified Flowbox.Luna.AST.Expr as Expr
import           Flowbox.Luna.AST.Expr   (Expr)
import qualified Flowbox.Luna.AST.Type as Type


mk :: String -> String -> Expr
mk name' param' = Expr.Field name' (Type.Sig param')