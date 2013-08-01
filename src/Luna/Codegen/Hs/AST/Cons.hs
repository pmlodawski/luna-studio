---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Cons (
    Cons(..),
    empty,
    genCode,
)where

import           Data.String.Utils                 (join)

import qualified Luna.Codegen.Hs.AST.Field       as Field
import           Luna.Codegen.Hs.AST.Field         (Field)


data Cons     = Cons { name :: String, fields :: [Field] } deriving (Show)


empty :: Cons
empty = Cons "" []

--genCode :: GenContext -> Function -> String
genCode cons = name cons ++ " { " ++ join ", " (map Field.genCode (fields cons)) ++ " }"


