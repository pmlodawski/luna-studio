---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.HAST.Module (
	module Flowbox.Luna.Data.HAST.Module,
	module Flowbox.Luna.Data.HAST.Expr
)where

import           Flowbox.Prelude               
import           Flowbox.Luna.Data.HAST.Expr   


empty :: Expr 
empty = Module [] [] [] []

mk :: [String] -> Expr
mk path' = Module path' [] [] [] 

addImport path mod = mod { imports = (Import False path Nothing) : imports mod }