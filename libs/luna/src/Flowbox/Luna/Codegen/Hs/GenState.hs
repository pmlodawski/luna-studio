---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Codegen.Hs.GenState where

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

logger = getLogger "Flowbox.Luna.Codegen.Hs.Generator"

data GenState = GenState { varcount :: Int
                         , varmap   :: Map String String
                         } deriving (Show)


type Generator m = (Functor m, MonadState GenState m, MonadWriter [LogEntry.LogEntry] m)


empty :: GenState
empty = GenState 0 Map.empty


genVarName :: Generator m => MaybeT m String
genVarName = do
    state <- get
    let vname = "v'" ++ show (varcount state)
    put $ state{ varcount = 1 + varcount state }
    return vname


registerVar :: Generator m => (String, String) -> MaybeT m ()
registerVar (alias, vname) = do
    state <- get
    put $ state { varmap = Map.insert alias vname $ varmap state }
    return ()

lookupVar :: Generator m => String -> MaybeT m (Maybe String)
lookupVar vname = do
    state <- get
    return $ Map.lookup vname (varmap state)


uniqueVar :: Generator m => String -> MaybeT m String
uniqueVar vname = do
    v <- lookupVar vname
    case v of
        Nothing      -> return vname
        Just oldname -> genVarName

handleVar :: Generator m => String -> MaybeT m String
handleVar vname = do
    newname <- uniqueVar vname
    registerVar (vname, newname)
    return newname

registerVars :: Generator m => [(String, String)] -> MaybeT m ()
registerVars vars = mapM_ registerVar vars
