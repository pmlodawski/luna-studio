---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Luna.Passes.Analysis.CallGraph.State where

import Control.Applicative

import           Control.Monad.State (MonadState, get, modify, put)

import           Flowbox.Prelude                            hiding (error, id, mod)
import           Flowbox.System.Log.Logger                  hiding (info)
import qualified Flowbox.Luna.Data.Pass.CallGraph as CallGraph
import           Flowbox.Luna.Data.Pass.CallGraph (CallGraph)
import           Flowbox.Luna.Data.AST.AST        (AST, ID)
import           Flowbox.Luna.Data.Pass.AliasInfo                               (AliasInfo)
import qualified Flowbox.Luna.Data.Pass.AliasInfo                               as AliasInfo

logger :: LoggerIO
logger = getLoggerIO "Flowbox.Luna.Passes.CallGraph.State"


data State = State { _cg      :: CallGraph 
                   , _info    :: AliasInfo
                   , _idStack :: [ID]
                   }
           deriving (Show)


makeLenses (''State)


type CGMonad m = (MonadState State m, Applicative m)


mk :: AliasInfo -> State
mk i = mempty & info .~ i

getCurrentID :: CGMonad m => m (Maybe ID)
getCurrentID = do stack <- view idStack <$> get
                  return $ case stack of
                      []    -> Nothing
                      (x:_) -> Just x

getInfo :: CGMonad m => m AliasInfo
getInfo = view info <$> get

registerFunction :: CGMonad m => ID -> m ()
registerFunction id = modify (cg %~ CallGraph.insert id)


registerCall :: CGMonad m => ID -> m ()
registerCall id = do
    mCid <- getCurrentID
    case mCid of 
        Nothing  -> return ()
        Just cid -> modify (cg %~ CallGraph.connect (cid, id))


pushID :: CGMonad m => ID -> m ()
pushID id = modify (idStack %~ (id:))

popID :: CGMonad m => m (Maybe ID)
popID = do stack <- view idStack <$> get
           case stack of
               (id:ids) -> do modify (idStack .~ ids)
                              return $ Just id
               []       -> return Nothing


withID :: CGMonad m => ID -> m f -> m f
withID id f = pushID id *> f <* popID


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Monoid State where
    mempty = State mempty mempty mempty
    --mappend a b = State 