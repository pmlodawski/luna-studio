---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.State where

import           Flowbox.Prelude                   
import           Control.Monad.State               
import qualified Flowbox.Luna.Data.AliasAnalysis as AA
import           Flowbox.Luna.Data.AliasAnalysis   (AA)
import qualified Data.Map                        as Map
import           Data.Map                          (Map)

import qualified Data.IntMap                     as IntMap

import           Flowbox.System.Log.Logger         


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"



data LocState    = LocState { namemap :: Map String Int
                            , varstat :: AA
                            } deriving (Show)

type LocStateM m = MonadState LocState m


empty :: LocState
empty = LocState Map.empty AA.empty


bind :: LocStateM m => Int -> Int -> m ()
bind kid vid = do
    s <- get
    let vs = varstat s
    let nvs = vs { AA.varmap = IntMap.insert kid vid $ AA.varmap vs }
    put s { varstat = nvs }


updateVarStat :: LocStateM m => LocState -> m ()
updateVarStat ns = do
    let nvs = varstat ns
    s <- get
    put $ s { varstat = nvs }


registerVarName :: LocStateM m => (String, Int) -> m ()
registerVarName (alias, vname) = do
    s <- get
    put $ s { namemap = Map.insert alias vname $ namemap s }


lookupVar :: LocStateM m => String -> m (Maybe Int)
lookupVar vname = do
    s <- get
    return $ Map.lookup vname (namemap s)
