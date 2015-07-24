module Reactive.Plugins.Core.Action.State.Breadcrumb where


import           Utils.PreludePlus
import           Utils.Vector
import           Widget.Button ( Button )


data State = State { _path        :: [Text]
                   , _buttons     :: [Button]
                   , _nextId      :: Int
                   } deriving (Eq, Show)

makeLenses ''State


instance Default State where
    def = State def def def

instance PrettyPrinter State where
    display (State path buttons nextId) = "cS(" <> display path <>
                                   " "   <> display buttons <>
                                   " "   <> display nextId <>
                                   " )"
