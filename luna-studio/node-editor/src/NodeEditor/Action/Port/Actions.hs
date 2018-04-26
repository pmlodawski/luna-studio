module NodeEditor.Action.Port.Actions
    ( handleMouseDown
    , handleClick
    ) where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           LunaStudio.Data.PortRef            (AnyPortRef (OutPortRef'), OutPortRef, nodeLoc, srcPortId)
import           LunaStudio.Data.ScreenPosition     (ScreenPosition)
import           NodeEditor.Action.Basic            (localAddPort)
import           NodeEditor.Action.Connect          (connectToPort, startConnecting)
import           NodeEditor.Action.Sidebar          (startPortDrag)
import           NodeEditor.React.Model.Node        (countProjectionPorts, hasPort)
import           NodeEditor.React.Model.Port        (getPortNumber)
import           NodeEditor.State.Action            (Action (continue), Mode (Click, Drag), connectAction, connectMode, portDragAction,
                                                     portDragMode)
import           NodeEditor.Action.State.Action     (checkAction, checkIfActionPerfoming)
import           NodeEditor.Action.State.NodeEditor (getInputNode)
import           NodeEditor.State.Global            (State)


handleMouseDown :: ScreenPosition -> AnyPortRef -> Command State ()
handleMouseDown mousePos portRef = do
    mayConnect  <- checkAction connectAction
    mayPortDrag <- checkAction portDragAction
    when ( Just Click /= (view connectMode  <$> mayConnect)
        && Just Click /= (view portDragMode <$> mayPortDrag) ) $
        startPortDragOrConnect mousePos portRef Drag

handleClick :: ScreenPosition -> AnyPortRef -> Command State ()
handleClick mosuePos portRef = do
    mayConnect <- checkAction connectAction
    newAction  <- not <$> checkIfActionPerfoming portDragAction
    if Just Click == (view connectMode <$> mayConnect) then continue $ connectToPort portRef
    else if newAction                                  then startPortDragOrConnect mosuePos portRef Click
    else return ()

startPortDragOrConnect :: ScreenPosition -> AnyPortRef -> Mode -> Command State ()
startPortDragOrConnect mousePos portRef mode = do
    mayInputNode <- getInputNode (portRef ^. nodeLoc)
    case (mayInputNode, portRef) of
        (Just _, OutPortRef' inPortRef) -> do
            isArgumentConstructor <- addArgumentConstructorIfPossibleAndNeeded inPortRef
            startPortDrag mousePos inPortRef isArgumentConstructor mode
        _ -> startConnecting mousePos portRef Nothing False mode

addArgumentConstructorIfPossibleAndNeeded :: OutPortRef -> Command State Bool
addArgumentConstructorIfPossibleAndNeeded portRef = do
    let nid = portRef ^. nodeLoc
        pid = portRef ^. srcPortId
    getInputNode nid >>= \case
        Nothing   -> return False
        Just node -> if hasPort pid node || countProjectionPorts node /= getPortNumber pid
            then return False
            else localAddPort portRef Nothing def
