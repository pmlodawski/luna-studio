---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Codegen.Hs.GenState where

import           Control.Monad.State   
import qualified Data.Map            as Map
import           Data.Map              (Map)


data GenState = GenState { varcount :: Int
                         , varmap   :: Map String String
                         } deriving (Show)

empty :: GenState
empty = GenState 0 Map.empty


genVarName :: State GenState String
genVarName = do
    state <- get
    let vname = "v''" ++ show (varcount state)
    put $ state{ varcount = 1 + varcount state }
    return vname


registerVar :: String -> String -> State GenState ()
registerVar alias vname = do
    state <- get
    put $ state { varmap = Map.insert alias vname $ varmap state }
    return ()
