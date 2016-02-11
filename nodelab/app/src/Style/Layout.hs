module Style.Layout where

import           Utils.PreludePlus
import           Utils.Vector

import qualified Object.Widget.Button as Button
import qualified Object.Widget.Group  as Group
import           Style.Types

sidebarPadding                 = 10.0
sidebarBackground              = (0.35, 0.35, 0.35)

sidebar                        = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ uniformPadding sidebarPadding
                                     & Group.borderRadius .~ (0, 0, 0, 0)

sidebarWidth                   = 0

breadcrumbBackground           = sidebarBackground
breadcrumbPosition             = Vector2 sidebarWidth 0.0

breadcrumbStyle                = def & Group.background   ?~ sidebarBackground
                                     & Group.padding      .~ xyPadding 10.0 5.0
                                     & Group.borderRadius .~ (0, 0, 0, 0)


breadcrumbItemSize             = Vector2 150.0 20.0
breadcrumbItemStyle            = def & Button.rounded   .~ False

projectChooser                 = def & Group.padding    .~ uniformPadding 5.0
                                     & Group.background ?~ sidebarBackground

projectListItemSize            = Vector2 190.0 20.0
createProjectButtonSize        = Vector2 200.0 20.0

createProjectDialogPosition    = Vector2 230.0 40.0
createProjectDialogStyle       = def & Group.background ?~ (0.3, 0.3, 0.5)
                                     & Group.padding    .~ uniformPadding 5.0

createProjectDialogTextBoxSize = Vector2 200.0 20.0

createProjectDialogOKSize      = Vector2 100.0 20.0
createProjectDialogCancelSize  = Vector2 80.0 20.0

projectChooserStyle = def & Group.background   ?~ (0.55, 0.55, 0.55)
                          & Group.padding      .~ uniformPadding 5.0
