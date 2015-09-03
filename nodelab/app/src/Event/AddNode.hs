module Event.AddNode where

import Utils.PreludePlus
import Object.Node

data Event = AddNode { _node :: Node } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (AddNode n) = "addN(" ++ display n ++ ")"

