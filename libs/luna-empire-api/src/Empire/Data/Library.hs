module Empire.Data.Library where

import Prologue
import System.Path        (Path)
import Data.Version       (Version)
import Empire.Data.Module (Module)

data Library = Library { _name    :: Maybe String
                       , _path    :: Path
                       , _version :: Version
                       , _body    :: Module
                       } deriving (Show)

makeLenses ''Library
