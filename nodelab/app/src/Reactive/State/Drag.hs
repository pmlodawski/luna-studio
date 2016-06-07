module Reactive.State.Drag where


import           Data.Aeson        (ToJSON)
import           Utils.PreludePlus
import           Utils.Vector

data DragHistory = DragHistory { _dragStartPos    :: Vector2 Int
                               , _dragPreviousPos :: Vector2 Int
                               , _dragCurrentPos  :: Vector2 Int
                               } deriving (Eq, Show, Generic)

data State = State { _history :: Maybe DragHistory
                   } deriving (Eq, Show, Generic)

makeLenses ''State
makeLenses ''DragHistory

instance ToJSON State
instance ToJSON DragHistory

instance Default State where
    def = State def
