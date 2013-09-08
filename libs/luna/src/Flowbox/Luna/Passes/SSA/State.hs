---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.State where

import           Flowbox.Prelude             
import           Control.Monad.State         
import qualified Data.Map                  as Map
import           Data.Map                    (Map)

import           Flowbox.System.Log.Logger   


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.SSA.State"


data SSAState = SSAState { varcount :: Int
                         , varmap   :: Map String String
                         } deriving (Show)


type SSAStateM m = MonadState SSAState m


empty :: SSAState
empty = SSAState 0 Map.empty


genVarName :: SSAStateM m => m String
genVarName = do
    s <- get
    let vname = "v'" ++ show (varcount s)
    put $ s{ varcount = 1 + varcount s }
    return vname


registerVar :: SSAStateM m => (String, String) -> m ()
registerVar (alias, vname) = do
    s <- get
    put $ s { varmap = Map.insert alias vname $ varmap s }
    return ()

lookupVar :: SSAStateM m => String -> m (Maybe String)
lookupVar vname = do
    s <- get
    return $ Map.lookup vname (varmap s)


uniqueVar :: SSAStateM m => String -> m String
uniqueVar vname = do
    v <- lookupVar vname
    case v of
        Nothing      -> return vname
        Just oldname -> genVarName

handleVar :: SSAStateM m => String -> m String
handleVar vname = do
    newname <- uniqueVar vname
    registerVar (vname, newname)
    return newname

registerVars :: SSAStateM m => [(String, String)] -> m ()
registerVars vars = mapM_ registerVar vars
