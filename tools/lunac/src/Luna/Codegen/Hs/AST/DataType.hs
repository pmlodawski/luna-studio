---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.DataType (
    DataType(..),
    empty,
    genCode,
    addDeriving
)where

import           Data.String.Utils                 (join)

import qualified Luna.Codegen.Hs.AST.Cons        as Cons
import           Luna.Codegen.Hs.AST.Cons          (Cons)
import qualified Luna.Codegen.Hs.AST.Deriving    as Deriving
import           Luna.Codegen.Hs.AST.Deriving      (Deriving)


data DataType = DataType { name         :: String,
                           typeparams   :: [String],
                           cons         :: [Cons],
                           derivings    :: [Deriving]
                         } deriving (Show)


empty :: DataType
empty = DataType "" [] [] []


genCode :: DataType -> String
genCode dt =  "data " ++ name dt ++ " " ++ join " " (typeparams dt) ++ " = " 
           ++ join " | "  (map Cons.genCode (cons dt))
           ++ Deriving.genCode (derivings dt)


addDeriving :: Deriving -> DataType -> DataType
addDeriving d dt = dt { derivings = d : (derivings dt)}


