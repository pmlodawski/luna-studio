---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Parser.AST.Field where

import           Flowbox.Luna.Parser.AST.AST 
import qualified Flowbox.Luna.Parser.AST.Type     as Type
import           Flowbox.Luna.Parser.AST.Type       (Type)


mk :: String -> String -> Expr
mk name' param' = Field name' (Type.Type param')