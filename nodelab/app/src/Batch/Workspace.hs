module Batch.Workspace where

import Utils.PreludePlus
import Batch.Project
import Batch.Library
import Batch.Breadcrumbs

data InterpreterState = Fresh
                      | InsertsInProgress Int
                      | AllSet
                      deriving (Show, Eq)

data Workspace = Workspace { _project          :: Project
                           , _library          :: Library
                           , _breadcrumbs      :: Breadcrumbs
                           , _interpreterState :: InterpreterState
                           , _shouldLayout     :: Bool
                           } deriving (Show, Eq)

makeLenses ''Workspace

addSerializationMode :: Int -> InterpreterState -> InterpreterState
addSerializationMode goal current = case current of
    Fresh -> InsertsInProgress 1
    InsertsInProgress x | (x + 1) == goal -> AllSet
                        | otherwise       -> InsertsInProgress $ x + 1
    AllSet -> AllSet

instance PrettyPrinter Workspace where
    display workspace = "Workspace(" <> show (workspace ^. project)
                              <> "," <> show (workspace ^. library)
                              <> "," <> show (workspace ^. breadcrumbs)
                              <> "," <> show (workspace ^. shouldLayout)
                              <> ")"
