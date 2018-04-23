module NodeEditor.View.NodeLoc
    ( module NodeEditor.View.NodeLoc
    , module X
    ) where

import           Common.Prelude
import           Data.Convert            (Convertible (convert))
import           LunaStudio.Data.NodeLoc as X (NodeLoc)

instance Convertible NodeLoc String where
    convert = show
