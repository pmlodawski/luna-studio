module Reactive.Plugins.Core.Action.State.Breadcrumb where


import           Utils.PreludePlus
import           Utils.Vector
import           Object.Widget.Button ( Button )

data State = State { _buttons :: [Int]
                   , _path :: [Text]
                   , _slider :: Maybe Int
                   } deriving (Eq, Show)

makeLenses ''State


instance Default State where
    def = State def def def

instance PrettyPrinter State where
    display (State _ path _) = "cBc(" <> display path <> " )"
