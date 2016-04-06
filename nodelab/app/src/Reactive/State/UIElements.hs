module Reactive.State.UIElements where


import           Utils.PreludePlus
import           Data.Aeson        (ToJSON)
import           Object.UITypes

data State = State { _sidebar           :: WidgetId
                   , _breadcrumbs       :: WidgetId
                   , _textEditorVisible :: Bool
                   } deriving (Eq, Show, Generic)


makeLenses ''State

instance ToJSON State

instance Default State where
    def = State def def True -- initialized in Init#initialize
