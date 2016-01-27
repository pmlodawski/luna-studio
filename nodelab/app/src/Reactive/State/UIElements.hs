module Reactive.State.UIElements where


import           Utils.PreludePlus
import           Data.Aeson        (ToJSON)
import           Object.UITypes

data State = State { _sidebar        :: WidgetId
                   , _projectChooser :: WidgetId
                   } deriving (Eq, Show, Generic)


makeLenses ''State

instance ToJSON State

instance Default State where
    def = State def def -- initialized in Init#initialize
