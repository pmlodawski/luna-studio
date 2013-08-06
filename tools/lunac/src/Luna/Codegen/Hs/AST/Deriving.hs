---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Deriving (
    Deriving(..),
    genCode
)where

import           Data.String.Utils                 (join)

data Deriving = Eq
              | Ord
              | Enum
              | Ix
              | Bounded
              | Read
              | Show
		    deriving (Show)


genCode :: [Deriving] -> String
genCode d = case d of
	[] -> ""
	_  -> " deriving (" ++ join ", " (map show d) ++ ")"









