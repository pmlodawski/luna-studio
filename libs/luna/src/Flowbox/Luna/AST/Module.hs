---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.AST.Module where

import           Flowbox.Prelude         
import           Flowbox.Luna.AST.AST    
import qualified Flowbox.Luna.AST.Type as Type


mk :: [Expr] -> Expr
mk body = Module NOP body