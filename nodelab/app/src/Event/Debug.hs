module Event.Debug where


import           Utils.PreludePlus


data Event = GetState deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display GetState = "GetState"

