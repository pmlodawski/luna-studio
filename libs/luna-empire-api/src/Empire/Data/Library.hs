module Empire.Data.Library where

import Prologue
import System.Path        (Path)
import Data.Version       (Version)
import Empire.Data.AST    (AST)

type LibraryId = Int

data Library = Library { _name    :: Maybe String
                       , _path    :: Path
                       , _body    :: AST
                       } deriving (Show)

make :: Maybe String -> Path -> Library
make name path = Library name path def

makeLenses ''Library
