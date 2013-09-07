---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.SSA.State where

import           Control.Monad.State           
import qualified Data.Map                    as Map
import           Data.Map                      (Map)
import           Control.Monad.State           
import           Control.Monad.Writer          
import           Control.Monad.RWS             
import           Control.Monad.Trans.Maybe     
import           Control.Monad.Trans.Either    

import           Flowbox.System.Log.Logger     
import qualified Flowbox.System.Log.LogEntry as LogEntry

logger = getLogger "Flowbox.Luna.Passes.SSA.State"

data SSAState = SSAState { varcount :: Int
                         , varmap   :: Map String String
                         } deriving (Show)


type SSAStateM m = MonadState SSAState m

empty :: SSAState
empty = SSAState 0 Map.empty


genVarName :: SSAStateM m => m String
genVarName = do
    state <- get
    let vname = "v'" ++ show (varcount state)
    put $ state{ varcount = 1 + varcount state }
    return vname


registerVar :: SSAStateM m => (String, String) -> m ()
registerVar (alias, vname) = do
    state <- get
    put $ state { varmap = Map.insert alias vname $ varmap state }
    return ()

lookupVar :: SSAStateM m => String -> m (Maybe String)
lookupVar vname = do
    state <- get
    return $ Map.lookup vname (varmap state)


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
