{-# LANGUAGE OverloadedStrings #-}

module Style.Layout where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Object.Widget.Button as Button
import qualified Object.Widget.Group  as Group
import qualified Object.Widget.Label  as Label
import           Style.Types

sidebarPadding                 = 10.0
sidebarBackground              = Color 0.35 0.35 0.35 1.0

sidebar                        = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ uniformPadding sidebarPadding
                                     & Group.borderRadius .~ (0, 0, 0, 0)

sidebarWidth                   = 0

breadcrumbBackground           = sidebarBackground
breadcrumbPosition             = Vector2 sidebarWidth 0.0

breadcrumbStyle                = def & Group.background   ?~ Color 1.0 1.0 1.0 0.05
                                     & Group.padding      .~ xyPadding 5.0 0.0
                                     & Group.borderRadius .~ (0, 0, 0, 0)

breadcrumbsHeight              = 20.0

breadcrumbItemSize             = Vector2 150.0 20.0
breadcrumbItemStyle            = def & Button.rounded    .~ False
                                     & Button.background .~ transparent
                                     & Button.alignment  .~ Label.Left

projectChooser                 = def & Group.padding    .~ uniformPadding 5.0
                                     & Group.background ?~ sidebarBackground

projectListItemSize            = Vector2 190.0 20.0
createProjectButtonSize        = Vector2 200.0 20.0

createProjectDialogPosition    = Vector2 230.0 40.0
createProjectDialogStyle       = def & Group.background ?~ Color 0.3 0.3 0.5 1.0
                                     & Group.padding    .~ uniformPadding 5.0

createProjectDialogTextBoxSize = Vector2 200.0 20.0

createProjectDialogOKSize      = Vector2 100.0 20.0
createProjectDialogCancelSize  = Vector2 80.0 20.0

projectChooserStyle = def & Group.background   ?~ Color 0.55 0.55 0.55 1.0
                          & Group.padding      .~ uniformPadding 5.0

textEditorToggle = Button.create (Vector2 10 1000) ":" & Button.style .~ style where
     style = def & Button.rounded    .~ False
                 & Button.background .~ Color 0.1 0.1 0.1 1.0
                 & Button.alignment  .~ Label.Center

--TODO[PM] : move it to proper place
inputsEdgePosition = Vector2 sidebarWidth 400.0
--TODO[PM] : move it to proper place
outputsEdgePosition = Vector2 1000.0 400.0

--TODO[PM] : style it
outputsEdgeStyle = def & Group.background   ?~ Color 0.10 0.10 0.10 1.0
                       & Group.padding      .~ uniformPadding 5.0
--TODO[PM] : style it
inputsEdgeStyle = def & Group.background   ?~ Color 0.10 0.10 0.10 1.0
                      & Group.padding      .~ uniformPadding 5.0


errorMessageWrapMargin = 30 :: Int
errorMessageWidgetSize  = Vector2 200 200 :: Vector2 Double
visualizationWidgetSize = Vector2 200 200 :: Vector2 Double
dataFrameWidgetSize     = Vector2 400 200 :: Vector2 Double
