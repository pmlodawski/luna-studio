module Reactive.State.UIElements where


import           Utils.PreludePlus
import           Data.Aeson        (ToJSON)
import           Object.UITypes

data ProjectChooser = ProjectChooser { _pcContainer :: WidgetId
                                     , _pcList      :: WidgetId
                                     } deriving (Eq, Show, Generic)

data State = State { _sidebar        :: WidgetId
                   , _projectChooser :: ProjectChooser
                   , _breadcrumbs    :: WidgetId
                   } deriving (Eq, Show, Generic)


makeLenses ''State
makeLenses ''ProjectChooser

instance ToJSON State
instance ToJSON ProjectChooser

instance Default State where
    def = State def def def -- initialized in Init#initialize

instance Default ProjectChooser where
    def = ProjectChooser def def -- initialized in Init#initialize
