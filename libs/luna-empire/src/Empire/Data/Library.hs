module Empire.Data.Library where

import Prologue
import System.Path        (Path)
import Empire.Data.Graph  (Graph)
import qualified Empire.API.Data.Library as API

data Library = Library { _name    :: Maybe String
                       , _path    :: Path
                       , _body    :: Graph
                       } deriving (Show)

make :: Maybe String -> Path -> Library
make name path = Library name path def

makeLenses ''Library

toAPI :: Library -> API.Library
toAPI (Library n p _) = API.Library n (show p)
