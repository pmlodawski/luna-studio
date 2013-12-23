---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}

module Flowbox.Luna.Passes.Analysis.VarAlias.State where

import           Control.Monad.State
import qualified Data.IntMap         as IntMap
import           Data.Map            (Map)
import qualified Data.Map            as Map

import           Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap (GeneralVarMap)
import qualified Flowbox.Luna.Data.Analysis.Alias.GeneralVarMap as GeneralVarMap
import           Flowbox.Prelude                                hiding (id)
import           Flowbox.System.Log.Logger



logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VarAlias.State"


data LocState    = LocState { namemap :: Map String Int
                            , varstat :: GeneralVarMap
                            } deriving (Show)

type LocStateM m = MonadState LocState m


empty :: LocState
empty = LocState Map.empty GeneralVarMap.empty


bind :: LocStateM m => Int -> Either String Int -> m ()
bind kid vid = do
    s <- get
    let vs  = varstat s
        nvs = vs { GeneralVarMap.varmap = IntMap.insert kid vid $ GeneralVarMap.varmap vs }
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


bindVar :: LocStateM m => String -> Int -> m ()
bindVar name id = do mv <- lookupVar name
                     case mv of
                        Just v  -> bind id $ Right v
                        Nothing -> bind id $ Left $ "Not in scope: " ++ (show name)