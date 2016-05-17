module Empire.API.Graph.Request where

import           Prologue
import           Empire.API.Data.GraphLocation (GraphLocation)

class GraphRequest a where
  location :: Lens' a GraphLocation
