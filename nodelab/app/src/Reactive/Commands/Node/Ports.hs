{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Ports
    ( displayPorts
    ) where

import           Control.Monad.State                 hiding (State)
import qualified Data.Map.Lazy                       as Map
import qualified Data.Text.Lazy                      as Text
import           Utils.PreludePlus
import           Utils.Vector

import           Object.UITypes                      (WidgetId)
import qualified Object.Widget.Label                 as Label
import qualified Object.Widget.Node                  as Model
import qualified Object.Widget.Port                  as PortModel
import qualified UI.Handlers.Node                    as Node

import           Reactive.Commands.Command           (Command, performIO)
import           Reactive.Commands.Graph             (colorPort, portDefaultAngle)
import           Reactive.Commands.Node.PortControls (makePortControl)
import qualified Reactive.Commands.UIRegistry        as UICmd
import qualified Reactive.State.UIRegistry           as UIRegistry

import           Empire.API.Data.Node                (Node, NodeId)
import qualified Empire.API.Data.Node                as Node
import           Empire.API.Data.Port                (InPort (..), InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port                as Port
import           Empire.API.Data.PortRef             (toAnyPortRef)
import           Empire.API.Data.ValueType           (ValueType (..))
import qualified Empire.API.Data.ValueType           as ValueType

nodePorts :: WidgetId -> Command UIRegistry.State [WidgetId]
nodePorts id = do
    children <- UICmd.children id
    let isPort id = (UIRegistry.lookupTypedM id :: UIRegistry.LookupFor PortModel.Port) >>= return . isJust
    filterM isPort children

makePorts :: Node -> [PortModel.Port]
makePorts node = makePort <$> ports where
    nodeId  = node ^. Node.nodeId
    makePort port = PortModel.Port portRef angle (portCount portId) isOnly (colorPort port) False where
        portRef = toAnyPortRef nodeId portId
        angle   = portDefaultAngle (portCount portId) (port ^. Port.portId)
        portId  = port ^. Port.portId
        isOnly  = 0 == portCount (InPortId Self)
    ports = Map.elems $ node ^. Node.ports
    portIds = Map.keys $  node ^. Node.ports
    portCount :: PortId -> Int
    portCount (OutPortId _) = sum $ fmap isOut portIds where
        isOut :: PortId -> Int
        isOut (OutPortId _) = 1
        isOut (InPortId  _) = 0
    portCount (InPortId  _) = sum $ fmap isIn  portIds where
        isIn :: PortId -> Int
        isIn (OutPortId _) = 0
        isIn (InPortId (Arg _)) = 1
        isIn (InPortId Self) = 0

displayPorts :: WidgetId -> Node -> Command UIRegistry.State ()
displayPorts id node = do
    nodeId <- UICmd.get id Model.nodeId
    oldPorts <- nodePorts id
    mapM_ UICmd.removeWidget oldPorts

    groupId <- Node.portControlsGroupId id
    portControls <- UICmd.children groupId
    mapM_ UICmd.removeWidget portControls

    outPortGroupId <- Node.outPortControlsGroupId id
    outPortControls <- UICmd.children outPortGroupId
    mapM_ UICmd.removeWidget outPortControls

    inLabelsGroupId <- Node.inLabelsGroupId id
    inLabels <- UICmd.children inLabelsGroupId
    mapM_ UICmd.removeWidget inLabels

    outLabelsGroupId <- Node.outLabelsGroupId id
    outLabels <- UICmd.children outLabelsGroupId
    mapM_ UICmd.removeWidget outLabels

    let newPorts = makePorts node

    forM_ newPorts $ \p -> UICmd.register id p def
    forM_ (node ^. Node.ports) $ \p -> makePortControl node outPortGroupId groupId (node ^. Node.nodeId) p
    forM_ (node ^. Node.ports) $ \p -> case p ^. Port.portId of
        InPortId  Self -> return ()
        InPortId  _    -> makePortLabel inLabelsGroupId p
        OutPortId _    -> makePortLabel outLabelsGroupId p

vtToText :: Getter ValueType Text
vtToText = to $ \v -> case v of
    ValueType.AnyType     -> "*"
    ValueType.TypeIdent a -> Text.pack $ toString a

makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
makePortLabel parent port = do
    let align = case port ^. Port.portId of
            InPortId  _ -> Label.Right
            OutPortId _ -> Label.Left
        label = Label.create (Vector2 360 15) text & Label.alignment .~ align
        text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
        portType = port ^. Port.valueType . vtToText
    UICmd.register_ parent label def
