module Reactive.Plugins.Core.Action.State.Graph where


import           Utils.PreludePlus
import           Utils.Vector


import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap



type NodesMap = IntMap Meta


data State = State { _nodes :: NodesMap
                   } deriving (Eq, Show)


instance Show State where
    show a = show $ IntMap.size $ a ^. nodes

instance Default State where
    def = State def

instance PrettyPrinter State where
    display (State nodes) =
           "nM("        <> show (IntMap.keys nodes)
        <> ")"
