---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Field (
    Field(..),
    empty,
    genCode
)where


data Field    = Field { name :: String, t :: String} deriving (Show)


empty :: Field
empty = Field "" ""


genCode :: Field -> [Char]
genCode field =  name field ++ " :: " ++ t field


