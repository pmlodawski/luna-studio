module Style.Node where

import           Utils.PreludePlus
import           Utils.Vector

import           Style.Types

import qualified Object.Widget.Group as Group
import qualified Object.Widget.Label as Label
import qualified UI.Layout           as Layout

nodeRadius = 30.0

expressionLabel = Label.Label position size align  where
    position = Vector2 (-150.0) (-50.0)
    size     = Vector2 300.0 20.0
    align    = Label.Center

nameLabel = Label.Label position size align  where
    position = Vector2 (-40.0) (50.0)
    size     = Vector2 80.0 20.0
    align    = Label.Center

controlsPosition = Vector2 (-nodeRadius) 50.0
controlsLayout   = Layout.verticalLayoutHandler 5.0

expandedGroupStyle = def & Group.background ?~ (0.2, 0.2, 0.2)
                         & Group.padding .~ uniformPadding 5.0

-- controlsPosition = Vector2 (-30.0) (-30.0)
-- controlsLayout   = Layout.verticalLayoutHandler 5.0
--
-- expandedGroupStyle = def & Group.background ?~ (0.2, 0.2, 0.2)
--                          & Group.padding .~ (Padding 70.0 0.0 10.0 0.0)
--                          & Group.borderRadius .~ (10.0, 10.0, 30.0, 10.0)

expandedGroupLayout = Layout.verticalLayoutHandler 5.0

valueLabel = Label.Label position size align  where
    position = Vector2 0 0
    size     = Vector2 100.0 20.0
    align    = Label.Left

visualizationGroupStyle = def & Group.background ?~ (0.2, 0.2, 0.2)
plotSize = Vector2 200.0 150.0

portControlSize = Vector2 200.0 20.0

setLabelSize  = Vector2 (0.7 * (portControlSize ^. x) - setLabelOffsetX) (portControlSize ^. y)
setButtonSize = Vector2 (0.3 * (portControlSize ^. x)) (portControlSize ^. y)

setLabelOffsetX = 10.0
labeledPadding = xyPadding setLabelOffsetX 0.0
