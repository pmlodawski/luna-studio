---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.Context where

import           Data.Map                           (Map)
import qualified Data.Map                           as Map

import           Flowbox.Bus.Data.Message           (Message)
import           Flowbox.Prelude                    hiding (Context)


type Stack = [([Message], Message)]

type Context = Map Int ProjectContext

data ProjectContext = ProjectContext { _undo :: Stack, _redo :: Stack }
                                     deriving (Show)

makeLenses ''ProjectContext


mk :: Context
mk = Map.empty
