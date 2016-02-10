module Style.Layout where

import Utils.PreludePlus
import Utils.Vector
import Style.Types
import qualified Object.Widget.Group as Group


sidebarPadding                 = 10.0
sidebarBackground              = (0.64, 0.21, 0.26)

sidebar                        = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ uniformPadding sidebarPadding
                                     & Group.borderRadius .~ (0, 0, 0, 0)

sidebarWidth                   = 200

breadcrumbBackground           = sidebarBackground
breadcrumbPosition             = Vector2 (sidebarWidth + 2 * sidebarPadding) 0

breadcrumbStyle                = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ xyPadding 10.0 5.0
                                     & Group.borderRadius .~ (0, 0, 0, 0)


projectChooser                 = def & Group.padding    .~ uniformPadding 5.0
                                     & Group.background ?~ (0.14, 0.42, 0.37)

projectListItemSize            = Vector2 190.0 20.0
createProjectButtonSize        = Vector2 200.0 20.0

createProjectDialogPosition    = Vector2 230.0 40.0
createProjectDialogStyle       = def & Group.background ?~ (0.3, 0.3, 0.5)
                               & Group.padding .~ uniformPadding 5.0

createProjectDialogTextBoxSize = Vector2 200.0 20.0

createProjectDialogOKSize      = Vector2 100.0 20.0
createProjectDialogCancelSize  = Vector2 80.0 20.0
