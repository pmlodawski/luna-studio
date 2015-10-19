module Object.UITypes where

import Utils.PreludePlus

type WidgetId    = Int

data SceneType   = HUD
                 | Workspace
                 deriving (Show, Eq, Enum)


