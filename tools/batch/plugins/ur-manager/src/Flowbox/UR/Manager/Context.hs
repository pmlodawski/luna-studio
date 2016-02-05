---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.UR.Manager.Context where

import           Data.Map                 (Map)
import qualified Data.Map                 as Map

import           Flowbox.Bus.Data.Message (Message)
import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Prelude          hiding (Context)


--type Actions = ([Message], Message)

data Actions  = Actions  { _undo :: [Message]     , _redo :: Message } deriving (Show)

data Metadata = Metadata { _description :: String , _cid :: Maybe Message.RequestID } deriving (Show)

data Package  = Package  { _actions :: [Actions]  , _metadata :: Metadata } deriving (Show)
--type Package  = ([Actions], String, Maybe Message.RequestID)

type Stack = [Package]

type Trans = [Metadata]

data ProjectContext = ProjectContext { _undoStack :: Stack
                                     , _redoStack :: Stack
                                     , _trans     :: Trans
                                     }
                                     deriving (Show)

makeLenses ''ProjectContext
makeLenses ''Package
makeLenses ''Metadata
makeLenses ''Actions

type Context = Map Int ProjectContext

-- do wywalenia, uzywaj mempty
mk :: Context
mk = Map.empty

instance Default ProjectContext where
    def = ProjectContext [] [] []
