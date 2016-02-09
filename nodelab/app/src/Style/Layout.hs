module Style.Layout where

import Utils.PreludePlus
import Style.Types
import qualified Object.Widget.Group as Group


projectChooser = def & Group.padding    .~ uniformPadding 5.0
                     & Group.background ?~ (0.14, 0.42, 0.37)


sidebar = def & Group.background ?~ (0.64, 0.21, 0.26)
              & Group.padding    .~ uniformPadding 5.0
