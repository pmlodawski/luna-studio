---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.DEP.Pass.Transform.AST.IDFixer.State where

import Control.Monad.State

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Common       as AST
import           Luna.DEP.Data.ASTInfo     (ASTInfo)
import qualified Luna.DEP.Data.ASTInfo     as ASTInfo



logger :: Logger
logger = getLogger $moduleName


data IDFixerState = IDFixerState { astInfo :: ASTInfo
                                 , rootID  :: Maybe AST.ID
                                 , fixAll  :: Bool
                                 } deriving (Show)


type IDFixerStateM m = (MonadState IDFixerState m, Functor m)


mk :: ASTInfo -> Maybe AST.ID -> Bool -> IDFixerState
mk = IDFixerState


getASTInfo :: IDFixerStateM m => m ASTInfo
getASTInfo = liftM astInfo get


setASTInfo :: IDFixerStateM m => ASTInfo -> m ()
setASTInfo i = do s <- get
                  put s { astInfo = i }


getFixAll :: IDFixerStateM m => m Bool
getFixAll = liftM fixAll get


getRootIDOnce :: IDFixerStateM m => m (Maybe AST.ID)
getRootIDOnce = do s <- get
                   put s { rootID = Nothing }
                   return $ rootID s


newID :: IDFixerStateM m => m AST.ID
newID = do n <- ASTInfo.incID <$> getASTInfo
           setASTInfo n
           return $ n ^. ASTInfo.lastID


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
