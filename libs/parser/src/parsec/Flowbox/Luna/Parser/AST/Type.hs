---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Parser.AST.Type where

data Type = Type  { name :: String                     }
	      | Class { name :: String, params :: [String] }
          deriving (Show, Eq)