---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Transform.AST.IDFixer.State where

import Control.Monad.State

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.AST.Common           as AST



logger :: Logger
logger = getLogger $(moduleName)


data IDFixerState = IDFixerState { maxID  :: AST.ID
                                 , rootID :: Maybe AST.ID
                                 , fixAll :: Bool
                                 } deriving (Show)


type IDFixerStateM m = MonadState IDFixerState m


make :: AST.ID -> Maybe AST.ID -> Bool -> IDFixerState
make = IDFixerState


getMaxID :: IDFixerStateM m => m AST.ID
getMaxID = liftM maxID get


setMaxID :: IDFixerStateM m => AST.ID -> m ()
setMaxID i = do s <- get
                put s { maxID = i }


getFixAll :: IDFixerStateM m => m Bool
getFixAll = liftM fixAll get


getRootIDOnce :: IDFixerStateM m => m (Maybe AST.ID)
getRootIDOnce = do s <- get
                   put s { rootID = Nothing }
                   return $ rootID s


newID :: IDFixerStateM m => m AST.ID
newID = do i <- getMaxID
           let n = i + 1
           setMaxID n
           return n


fixID :: IDFixerStateM m => AST.ID -> m AST.ID
fixID i = do
    r <- getRootIDOnce
    case r of
        Just rid  -> return rid
        Nothing   -> do f <- getFixAll
                        if f || i == unknownID
                            then newID
                            else return i


unknownID :: AST.ID
unknownID = -1
