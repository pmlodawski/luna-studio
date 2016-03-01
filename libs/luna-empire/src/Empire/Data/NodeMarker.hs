module Empire.Data.NodeMarker where

import           Prologue
import           Empire.API.Data.Node (NodeId)
import           Data.HMap.Lazy       (TypeKey (..))

newtype NodeMarker = NodeMarker NodeId deriving (Show, Eq)
makeWrapped ''NodeMarker

nodeMarkerKey :: TypeKey NodeMarker
nodeMarkerKey = TypeKey

