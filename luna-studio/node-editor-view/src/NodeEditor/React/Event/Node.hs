{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Event.Node
    ( module NodeEditor.React.Event.Node
    , nodeLoc
    ) where

import           Common.Data.Event       (EventName, eventName)
import           Common.Prelude
import           LunaStudio.Data.NodeLoc (HasNodeLoc (nodeLoc), NodeLoc)
import           React.Flux              (MouseEvent)


data Event = Event
    { _nodeLoc' :: NodeLoc
    , _evtType  :: EventType
    } deriving (Show, Generic, NFData, Typeable)

data EventType
    = MouseEnter
    | MouseLeave
    | MouseDown       MouseEvent
    | Enter
    | EditName
    | EditExpression
    | SetExpression   Text
    | Select          MouseEvent
    deriving (Show, Generic, NFData, Typeable)

makeLenses ''Event
instance EventName EventType
instance EventName Event where
    eventName e = eventName $ e ^. evtType
instance HasNodeLoc Event where nodeLoc = nodeLoc'
