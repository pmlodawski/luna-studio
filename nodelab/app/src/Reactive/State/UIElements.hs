module Reactive.State.UIElements where


import           Object.UITypes
import           Utils.PreludePlus


import           Data.Aeson        (ToJSON)

data State = State { _projectChooser :: WidgetId
                   } deriving (Eq, Show, Generic)


makeLenses ''State

instance ToJSON State

instance Default State where
    def = State def
