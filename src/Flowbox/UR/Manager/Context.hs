---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.Context where

import           Flowbox.Bus.Data.Message           (Message)
import           Flowbox.Prelude                    hiding (Context)


type Context2 = [Message]
data Context = Context { _undo :: [Message], _redo :: [Message] }
                       deriving (Show)

makeLenses ''Context


mk :: Context
mk =  Context [] []
