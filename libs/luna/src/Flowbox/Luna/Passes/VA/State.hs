---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.VA.State where

import           Flowbox.Prelude             
import           Control.Monad.State         
import qualified Data.Map                  as Map
import           Data.Map                    (Map)

import           Flowbox.System.Log.Logger   


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.VA.State"


data LocState = LocState { namemap :: Map String Int
                         , varstat :: VarStat
                         } deriving (Show)


data VarStat = VarStat { varmap :: Map Int Int } deriving (Show)


type LocStateM m = MonadState LocState m


empty :: LocState
empty = LocState Map.empty (VarStat Map.empty)


--genVarName :: LocStateM m => m String
--genVarName = do
--    s <- get
--    let vname = "v'" ++ show (varcount s)
--    put $ s{ varcount = 1 + varcount s }
--    return vname

bind kid vid = do
    s <- get
    let vs = varstat s
    let nvs = vs { varmap = Map.insert kid vid $ varmap vs }
    put s { varstat = nvs }

updateVarStat ns = do
    let nvs = varstat ns
    s <- get
    put $ s { varstat = nvs }

--registerVar vid = do
--    s <- get
--    put s { vars = vid : vars s }

registerVarName :: LocStateM m => (String, Int) -> m ()
registerVarName (alias, vname) = do
    s <- get
    put $ s { namemap = Map.insert alias vname $ namemap s }

lookupVar :: LocStateM m => String -> m (Maybe Int)
lookupVar vname = do
    s <- get
    return $ Map.lookup vname (namemap s)


--uniqueVar :: LocStateM m => String -> m String
--uniqueVar vname = do
--    v <- lookupVar vname
--    case v of
--        Nothing      -> return vname
--        Just oldname -> genVarName

--handleVar :: LocStateM m => String -> m String
--handleVar vname = do
--    newname <- uniqueVar vname
--    registerVarName (vname, newname)
--    return newname

--registerVars :: LocStateM m => [(String, String)] -> m ()
--registerVars vars = mapM_ registerVarName vars
