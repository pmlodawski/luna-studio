module Batch.Workspace where

import Utils.PreludePlus
import Batch.Project
import Batch.Library
import Batch.Breadcrumbs
import Data.Aeson (ToJSON)

data InterpreterState = Fresh
                      | InsertsInProgress Int
                      | AllSet
                      deriving (Show, Eq, Generic)

data Workspace = Workspace { _project          :: Project
                           , _library          :: Library
                           , _breadcrumbs      :: Breadcrumbs
                           , _interpreterState :: InterpreterState
                           , _shouldLayout     :: Bool
                           } deriving (Show, Eq, Generic)

instance ToJSON InterpreterState
instance ToJSON Workspace

makeLenses ''Workspace

addSerializationMode :: Int -> InterpreterState -> InterpreterState
addSerializationMode goal current = case current of
    Fresh -> InsertsInProgress 1
    InsertsInProgress x | (x + 1) == goal -> AllSet
                        | otherwise       -> InsertsInProgress $ x + 1
    AllSet -> AllSet
