module Empire.API.Data.Library where

import Prologue

import Data.Aeson  (ToJSON)
import Data.Binary (Binary)

type LibraryId = Int

data Library = Library { _name    :: Maybe String
                       , _path    :: String
                       } deriving (Show, Eq, Generic)

makeLenses ''Library

instance Binary Library

instance ToJSON Library
