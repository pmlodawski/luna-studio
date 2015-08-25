module Object.UITypes where

import Utils.PreludePlus

type WidgetId    = Int
data MouseButton = NoButton
                 | LeftButton
                 | MiddleButton
                 | RightButton
                 deriving (Show, Eq)
data SceneType   = HUD
                 | Workspace
                 deriving (Show, Eq, Enum)
