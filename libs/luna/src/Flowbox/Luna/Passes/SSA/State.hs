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


data SSAState = SSAState { namemap  :: Map String Int
                         , vars     :: [Int]
                         , varmap   :: Map Int Int
                         } deriving (Show)


type SSAStateM m = MonadState SSAState m


empty :: SSAState
empty = SSAState Map.empty [] Map.empty


--genVarName :: SSAStateM m => m String
--genVarName = do
--    s <- get
--    let vname = "v'" ++ show (varcount s)
--    put $ s{ varcount = 1 + varcount s }
--    return vname

bind kid vid = do
    s <- get
    put s { varmap = Map.insert kid vid $ varmap s }

registerVar vid = do
    s <- get
    put s { vars = vid : vars s }

registerVarName :: SSAStateM m => (String, Int) -> m ()
registerVarName (alias, vname) = do
    s <- get
    put $ s { namemap = Map.insert alias vname $ namemap s }

lookupVar :: SSAStateM m => String -> m (Maybe Int)
lookupVar vname = do
    s <- get
    return $ Map.lookup vname (namemap s)


--uniqueVar :: SSAStateM m => String -> m String
--uniqueVar vname = do
--    v <- lookupVar vname
--    case v of
--        Nothing      -> return vname
--        Just oldname -> genVarName

--handleVar :: SSAStateM m => String -> m String
--handleVar vname = do
--    newname <- uniqueVar vname
--    registerVarName (vname, newname)
--    return newname

--registerVars :: SSAStateM m => [(String, String)] -> m ()
--registerVars vars = mapM_ registerVarName vars
