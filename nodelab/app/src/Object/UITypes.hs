module Object.UITypes where

import Utils.PreludePlus
import Data.Aeson (ToJSON)

type WidgetId    = Int

data SceneType   = HUD
                 | Workspace
                 deriving (Show, Eq, Enum, Generic)


instance ToJSON SceneType
