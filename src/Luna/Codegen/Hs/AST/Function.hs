---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Function (
    Function(..),
    empty
)where

data Function = Function { name       :: String
                         } deriving (Show)


empty :: Function
empty = Function ""

genCode :: Function -> String
genCode func = name func ++ " = "