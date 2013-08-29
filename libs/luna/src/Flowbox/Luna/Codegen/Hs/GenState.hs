---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.GenState where

import           Control.Monad.State   


data GenState = GenState { varcount :: Int
                         } deriving (Show)

empty :: GenState
empty = GenState 0


genVarName :: State GenState String
genVarName = do
    state <- get
    let vname = "v''" ++ show (varcount state)
    put $ state{varcount = 1 + varcount state}
    return vname