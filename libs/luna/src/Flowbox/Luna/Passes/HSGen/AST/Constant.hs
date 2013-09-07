---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Passes.HSGen.AST.Constant where

import qualified Flowbox.Prelude as Prelude   
import           Flowbox.Prelude hiding (String, Char, Integer)

data Constant = Integer Prelude.String
              | String  Prelude.String
              | Char    Prelude.Char
              deriving (Show, Eq)

genCode :: Constant -> Prelude.String
genCode e = case e of
	Integer val -> val
	String  val -> "\"" ++ val   ++ "\""
	Char    val -> "'"  ++ [val] ++ "'"