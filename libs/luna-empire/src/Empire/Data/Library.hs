module Empire.Data.Library where

import Prologue
import System.Path        (Path, native)
import Empire.Data.Graph  (Graph)
import qualified Data.Text.Lazy as Text
import qualified Empire.API.Data.Library as API
import qualified Empire.API.Data.Graph   as API (Graph)
import qualified Empire.API.Persistence.Library as Persistence

data Library = Library { _name    :: Maybe String
                       , _path    :: Path
                       , _body    :: Graph
                       } deriving (Show)

make :: Maybe String -> Path -> Library
make name path = Library name path def

makeLenses ''Library

toAPI :: Library -> API.Library
toAPI (Library n p _) = API.Library n (Text.unpack $ native p)

toPersistent :: Library -> API.Graph -> Persistence.Library
toPersistent (Library n p _) = Persistence.Library n (Text.unpack $ native p)
