module UI.Widget.List where


import           Object.Widget
import qualified Object.Widget.List            as Model

import           UI.Widget.Group ()

instance UIDisplayObject Model.List where
    createUI parentId id model = createUI parentId id (Model.toGroup model)
    updateUI id old model = updateUI id (Model.toGroup old) (Model.toGroup model)
