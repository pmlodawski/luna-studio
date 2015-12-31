module Reactive.State.MultiSelection where


import Utils.PreludePlus
import Utils.Vector

import Data.Aeson (ToJSON)

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show, Generic)


data State = State { _history  :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON DragHistory

instance Default State where
    def = State def
