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
import qualified Flowbox.Bus.Data.Message           as Message
import           Flowbox.Prelude                    hiding (Context)


type Actions = ([Message], Message)

type Transaction = ([Actions], [String])

type Package  = ([Actions], String, Maybe Message.RequestID)

type Stack = [Package]

type Trans = [(Message.RequestID, String)]

data ProjectContext = ProjectContext { _undo  :: Stack
                                     , _redo  :: Stack
                                     , _trans :: Trans
                                     }
                                     deriving (Show)

makeLenses ''ProjectContext

type Context = Map Int ProjectContext

-- do wywalenia, uzywaj mempty
mk :: Context
mk = Map.empty

emptyProjectContext :: ProjectContext
emptyProjectContext = ProjectContext [] [] []

-- FIXME
--instance Default ProjectContext where
--	def = ProjectContext [] [] []