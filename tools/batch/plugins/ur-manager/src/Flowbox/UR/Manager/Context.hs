---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.Context where

import           Data.DList                         (DList)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map

import           Flowbox.Bus.Data.Message           (Message)
import           Flowbox.Prelude                    hiding (Context)


type Actions = ([Message], Message)

type Transaction = (DList Actions, String)

type Stack = [Transaction]

type Context = Map Int ProjectContext

data ProjectContext = ProjectContext { _undo  :: Stack
                                     , _redo  :: Stack 
                                     , _trans :: Maybe Transaction
                                     }
                                     deriving (Show)

makeLenses ''ProjectContext


mk :: Context
mk = Map.empty

emptyProjectContext :: ProjectContext
emptyProjectContext = ProjectContext [] [] Nothing
