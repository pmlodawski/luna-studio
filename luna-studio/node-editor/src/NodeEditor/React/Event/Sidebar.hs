{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies   #-}

module NodeEditor.React.Event.Sidebar where

import           Common.Prelude
import           LunaStudio.Data.NodeLoc (NodeLoc)
import           LunaStudio.Data.PortRef (OutPortRef)
import           React.Flux              (MouseEvent)



data Event = AddPort           OutPortRef
           | MouseMove         MouseEvent NodeLoc
           | RemovePort        OutPortRef
           | EditPortName      OutPortRef
           | ToggleInputMode   NodeLoc
           | ToggleOutputMode  NodeLoc
           | UnfreezeSidebar   NodeLoc
            deriving (Show, Generic, NFData, Typeable)
