module Reactive.Plugins.Core.Action.State.Breadcrumb where


import           Utils.PreludePlus
import           Utils.Vector
import           Widget.Button ( Button )


data State = State { _path        :: [Text]
                   , _buttons     :: [Button]
                   } deriving (Eq, Show)

makeLenses ''State


instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State path buttons) = "cS(" <> display path <>
                                   " "   <> display buttons <>
                                   " )"
