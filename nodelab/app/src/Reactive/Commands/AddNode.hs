{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.AddNode
    ( addNode
    , addDummyNode
    , updateNode
    , updateNodeValue
    , updateNodeProfilingData
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Control.Monad.State               hiding (State)
import           Data.Hashable                     (hash)
import qualified Data.Map.Lazy                     as Map
import qualified Data.Text.Lazy                    as Text
import           Data.List.Split                   (wordsBy)
import           Data.List                         (intercalate)
import           GHC.Float                         (double2Float)

import           Object.UITypes                    (WidgetId)
import           Object.Widget                     (objectId, widget)
import qualified Object.Widget.Button              as Button
import qualified Object.Widget.Group               as Group
import qualified Object.Widget.Label               as Label
import           Object.Widget.LabeledTextBox      (LabeledTextBox (..))
import qualified Object.Widget.LabeledTextBox      as LabeledTextBox
import qualified Object.Widget.Node                as Model
import           Object.Widget.Number.Continuous   (ContinuousNumber (..))
import qualified Object.Widget.Number.Continuous   as ContinuousNumber
import           Object.Widget.Number.Discrete     (DiscreteNumber (..))
import qualified Object.Widget.Number.Discrete     as DiscreteNumber
import qualified Object.Widget.Plots.ScatterPlot   as ScatterPlot
import qualified Object.Widget.Plots.Image         as Image
import qualified Object.Widget.Port                as PortModel
import           Object.Widget.Toggle              (Toggle (..))
import qualified Object.Widget.Toggle              as Toggle
import qualified Object.Widget.LongText            as LongText
import qualified Object.Widget.DataFrame           as DataFrame
import qualified UI.Handlers.Button                as Button
import qualified UI.Handlers.LabeledTextBox        as LabeledTextBox
import qualified UI.Handlers.Node                  as Node
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber
import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import qualified UI.Handlers.Toggle                as Toggle

import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.EnterNode       (enterNode)
import           Reactive.Commands.Graph           (colorPort, focusNode, nodeIdToWidgetId, portDefaultAngle,
                                                    updateNodeMeta)
-- import           Reactive.Commands.PendingNode   (unrenderPending)
import           Reactive.Commands.RemoveNode      (removeSelectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import           Reactive.Commands.Selection       (selectedNodes)
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.UIRegistry         (addHandler, sceneGraphId)
import qualified Reactive.State.UIRegistry         as UIRegistry

import qualified BatchConnector.Commands           as BatchCmd
import qualified JS.NodeGraph                      as UI

import           Data.HMap.Lazy                    (HTMap)
import qualified Data.HMap.Lazy                    as HMap
import           Data.Map.Lazy                     (Map)
import qualified Data.Map.Lazy                     as Map
import           UI.Handlers.Generic               (ValueChangedHandler (..), triggerValueChanged)
import qualified UI.Handlers.Node                  as UINode
import           UI.Layout                         as Layout
import qualified UI.Registry                       as UIR
import qualified UI.Scene
import qualified UI.Widget                         as UIT

import qualified Style.Node                        as Style
import qualified Style.Types                       as Style

import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.API.Data.DefaultValue      (Value (..))
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.Port              (InPort (..), InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (AnyPortRef (..), InPortRef (..), toAnyPortRef)
import           Empire.API.Data.ValueType         (ValueType (..))
import qualified Empire.API.Data.ValueType         as ValueType
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult


addNode :: Node -> Command State ()
addNode node = do
    -- unrenderPending node
    zoom Global.graph $ modify (Graph.addNode node)
    widgetId <- zoom Global.uiRegistry $ registerNode node
    Node.selectNode' Node.performSelect widgetId

addDummyNode :: NodeMeta -> NodeId -> Command State ()
addDummyNode meta nodeId = do
    mayNode <- preuse $ Global.graph . Graph.nodes . ix nodeId
    case mayNode of
        Just _  -> return ()
        Nothing -> do
            let dummyNode = Node.Node nodeId "" (Node.ExpressionNode "") ports meta
                ports = Map.fromList [(OutPortId All, Port (OutPortId All) "All" AnyType Port.NotConnected), (InPortId Self, Port (InPortId Self) "self" AnyType Port.NotConnected)]
            addNode dummyNode


registerNode :: Node -> Command UIRegistry.State WidgetId
registerNode node = do
    let nodeModel = Model.fromNode node
    nodeWidget <- UICmd.register sceneGraphId nodeModel (nodeHandlers node)

    displayPorts nodeWidget node
    focusNode nodeWidget
    return nodeWidget

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

isLiteral :: Getter Node Bool
isLiteral = to $ isLiteral' where
    isLiteral' node = (0 == (sum $ fmap isIn' portIds)) where
        portIds = Map.keys $ node ^. Node.ports
        isIn' :: PortId -> Int
        isIn' (OutPortId _) = 0
        isIn' (InPortId  _) = 1


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
        InPortId  _ -> makePortLabel inLabelsGroupId p
        OutPortId _ -> makePortLabel outLabelsGroupId p

makePortLabel :: WidgetId -> Port -> Command UIRegistry.State ()
makePortLabel parent port = do
    let align = case port ^. Port.portId of
            InPortId  _ -> Label.Right
            OutPortId _ -> Label.Left
        label = Label.create (Vector2 160 15) text & Label.alignment .~ align
        text  = (Text.pack $ port ^. Port.name) <> " :: " <> portType
        portType = case port ^. Port.valueType of
            ValueType.AnyType -> "a"
            ValueType.TypeIdent a -> Text.pack a
    UICmd.register_ parent label def

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.RenameNodeHandler $ \_ nodeId name -> do
                      workspace <- use Global.workspace
                      performIO $ BatchCmd.renameNode workspace nodeId name)
                  $ addHandler (UINode.ChangeInputNodeTypeHandler $ \_ nodeId name -> do
                      workspace <- use Global.workspace
                      performIO $ BatchCmd.setInputNodeType workspace nodeId name)
                  $ addHandler (UINode.FocusNodeHandler  $ \id -> zoom Global.uiRegistry (focusNode id))
                  $ addHandler (UINode.ExpandNodeHandler $ zoom Global.uiRegistry expandSelectedNodes)
                  $ addEnterNodeHandler where
                        addEnterNodeHandler = case node ^. Node.nodeType of
                            Node.FunctionNode _ -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Function $ Text.unpack $ node ^. Node.name) mempty
                            Node.ModuleNode     -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Module   $ Text.unpack $ node ^. Node.name) mempty
                            _                   -> mempty

expandSelectedNodes :: Command UIRegistry.State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    forM_ sn $ \wf -> do
        let id = wf ^. objectId
        UICmd.update_ id update
        UICmd.moveBy  id (Vector2 0 0) -- FIXME: trigger moved handler for html widgets

updateNode :: Node -> Command State ()
updateNode node = do
    let nodeId  = node ^. Node.nodeId
    inGraph <- preuse $ Global.graph . Graph.nodesMap . ix nodeId
    case inGraph of
        Just existingNode -> updateExistingNode node
        Nothing           -> addNode            node

updateExistingNode :: Node -> Command State ()
updateExistingNode node = do
    let nodeId  = node ^. Node.nodeId
    maybeWidgetId <- inRegistry $ nodeIdToWidgetId nodeId
    zoom Global.graph $ modify (Graph.addNode node)
    forM_ maybeWidgetId $ \widgetId -> do
        updateNodeMeta nodeId $ node ^. Node.nodeMeta
        inRegistry $ displayPorts widgetId node

        case node ^. Node.nodeType of
            Node.ExpressionNode expression -> do
                inRegistry $ UICmd.update_ widgetId $ Model.expression .~ expression
            _ -> return ()
        -- TODO: obsluzyc to ze moga zniknac polaczenia

onValueChanged :: Typeable a => (a -> WidgetId -> Command Global.State ()) -> HTMap
onValueChanged h = addHandler (ValueChangedHandler h) mempty

makePortControl :: Node -> WidgetId -> WidgetId -> NodeId -> Port -> Command UIRegistry.State ()
makePortControl node outPortParent groupParent nodeId port = let portRef = toAnyPortRef nodeId $ port ^. Port.portId in
    case port ^. Port.portId of
        InPortId  (Arg ix) -> makeInPortControl groupParent portRef port
        OutPortId All      -> when (node ^. isLiteral) $ makeInPortControl groupParent portRef port -- TODO: change groupParent to outPortParent
        _ -> return ()

makeInPortControl :: WidgetId -> AnyPortRef -> Port -> Command UIRegistry.State ()
makeInPortControl parent portRef port = case port ^. Port.state of
    Port.NotConnected    -> do
        case port ^. Port.valueType . ValueType.toEnum of
            ValueType.Other -> do
                let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) <> " :: " <> (show $ port ^. Port.valueType) )
                           & Label.position . x .~ Style.setLabelOffsetX
                UICmd.register_ parent widget mempty
            otherwise -> do
                let group = Group.create & Group.style . Group.padding .~ Style.Padding 0.0 0.0 0.0 Style.setLabelOffsetX
                groupId <- UICmd.register parent group (Layout.horizontalLayoutHandler 0.0)
                let label  = Label.create Style.setLabelSize (Text.pack $ port ^. Port.name)
                           & Label.position . x .~ Style.setLabelOffsetX
                    button = Button.create Style.setButtonSize "not set"
                    zeroValue = case port ^. Port.valueType . ValueType.toEnum of
                        ValueType.DiscreteNumber -> DefaultValue.IntValue def
                        ValueType.ContinuousNumber -> DefaultValue.DoubleValue def
                        ValueType.String ->  DefaultValue.StringValue def
                        ValueType.Bool -> DefaultValue.BoolValue False
                        _              -> undefined
                    handlers = addHandler (Button.ClickedHandler $ \_ -> do
                        workspace <- use Global.workspace
                        performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ zeroValue)
                        ) mempty

                UICmd.register_ groupId label def
                UICmd.register_ groupId button handlers
    Port.Connected       -> do
        let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) <> " (connected)")
                   & Label.position . x .~ Style.setLabelOffsetX
        void $ UICmd.register parent widget def
    Port.WithDefault def -> void $ case port ^. Port.valueType . ValueType.toEnum of
        ValueType.DiscreteNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0 $ def ^? DefaultValue._Constant . DefaultValue._IntValue
                widget = DiscreteNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ DefaultValue.IntValue val)
            UICmd.register parent widget handlers
        ValueType.ContinuousNumber -> do
            let label = port ^. Port.name
                value = fromMaybe 0.0 $ def ^? DefaultValue._Constant . DefaultValue._DoubleValue
                widget = ContinuousNumber.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ DefaultValue.DoubleValue val)
            UICmd.register parent widget handlers
        ValueType.String -> do
            let label = port ^. Port.name
                value = fromMaybe "" $ def ^? DefaultValue._Constant . DefaultValue._StringValue
                widget = LabeledTextBox.create Style.portControlSize (Text.pack $ label) (Text.pack $ value)
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ DefaultValue.StringValue $ Text.unpack val)
            UICmd.register parent widget handlers
        ValueType.Bool -> do
            let label = port ^. Port.name
                value = fromMaybe True $ def ^? DefaultValue._Constant . DefaultValue._BoolValue
                widget = Toggle.create Style.portControlSize (Text.pack $ label) value
                handlers = onValueChanged $ \val _ -> do
                    workspace <- use Global.workspace
                    performIO $ BatchCmd.setDefaultValue workspace portRef (DefaultValue.Constant $ DefaultValue.BoolValue val)
            UICmd.register parent widget handlers
        ValueType.Other -> do
            let widget = Label.create (Style.portControlSize & x -~ Style.setLabelOffsetX) (Text.pack $ (port ^. Port.name) )
                       & Label.position . x .~ Style.setLabelOffsetX

            UICmd.register parent widget mempty


nodeValueToText :: Value -> Text
nodeValueToText (IntValue    v)      = Text.pack $ show v
nodeValueToText (DoubleValue v)      = Text.pack $ show v
nodeValueToText (BoolValue   v)      = Text.pack $ show v
nodeValueToText (StringValue v)      = Text.pack $ "\"" <> (if (length v > 10) then (take 10 v) <> "..." else v) <> "\""
nodeValueToText (IntList     v)      = Text.pack $ "Vector [" <> (show $ length v) <> "]"
nodeValueToText (DoubleList  v)      = Text.pack $ "Vector [" <> (show $ length v) <> "]"
nodeValueToText (BoolList    v)      = Text.pack $ "Vector [" <> (show $ length v) <> "]"
nodeValueToText (StringList  v)      = Text.pack $ "Vector [" <> (show $ length v) <> "]"
nodeValueToText (DoublePairList v)   = Text.pack $ "Vector2 [" <> (show $ length v) <> "]"
nodeValueToText (IntPairList v)      = Text.pack $ "Vector2 [" <> (show $ length v) <> "]"
nodeValueToText (Histogram   v)      = Text.pack $ "Hist [" <> (show $ length v) <> "]"
nodeValueToText (Image      dataUrl width height) = Text.pack $ "Image"

removeVisualization :: WidgetId -> Command UIRegistry.State ()
removeVisualization id = do
    groupId <- Node.valueGroupId id
    widgets <- UICmd.children groupId
    forM_ widgets UICmd.removeWidget

zipVector :: [Double] -> [Vector2 Double] -> [Vector2 Double]
zipVector (ax:xs) [] = zipVector xs [Vector2 0 ax]
zipVector (ax:xs) (acc:accs) = zipVector xs ((Vector2 (acc ^. x + 1.0) ax):acc:accs)
zipVector [] accs = accs

zipVectorInt :: [Int] -> [Vector2 Double] -> [Vector2 Double]
zipVectorInt (ax:xs) [] = zipVectorInt xs [Vector2 0 (fromIntegral ax)]
zipVectorInt (ax:xs) (acc:accs) = zipVectorInt xs ((Vector2 (acc ^. x + 1.0) (fromIntegral ax)):acc:accs)
zipVectorInt [] accs = accs


displayListTable :: WidgetId -> [Text] -> Command UIRegistry.State ()
displayListTable groupId col = do
    let idxs = Text.pack . show <$> take (length col) [1..]
        cols = [idxs, col]
        rows = transpose cols
        df = DataFrame.create Style.plotSize ["Index", "Value"] rows
    UICmd.register_ groupId df def

visualizeNodeValue :: WidgetId -> Value -> Command UIRegistry.State ()
visualizeNodeValue id (IntList v) = do
    groupId <- Node.valueGroupId id

    let dataPoints = zipVectorInt v []
        widget = ScatterPlot.create Style.plotSize
               & ScatterPlot.dataPoints .~ dataPoints
    UICmd.register_ groupId widget def

    displayListTable groupId $ Text.pack . show <$> v

visualizeNodeValue id (StringList v) = do
    groupId <- Node.valueGroupId id
    displayListTable groupId $ Text.pack . show <$> v


visualizeNodeValue id (DoubleList v) = do
    groupId <- Node.valueGroupId id

    let dataPoints = zipVector v []
        widget = ScatterPlot.create Style.plotSize
               & ScatterPlot.dataPoints .~ dataPoints
    UICmd.register_ groupId widget def

    displayListTable groupId $ Text.pack . show <$> v

visualizeNodeValue id (IntPairList v) = do
    groupId <- Node.valueGroupId id

    let dataPoints = (\(a,b) -> Vector2 (fromIntegral a) (fromIntegral b)) <$> v
        widget = ScatterPlot.create Style.plotSize
               & ScatterPlot.dataPoints .~ dataPoints
    UICmd.register_ groupId widget def

visualizeNodeValue id (DoublePairList v) = do
    groupId <- Node.valueGroupId id

    let dataPoints = uncurry Vector2 <$> v
        widget = ScatterPlot.create Style.plotSize
               & ScatterPlot.dataPoints .~ dataPoints
    UICmd.register_ groupId widget def

visualizeNodeValue id (Histogram v) = do
    groupId <- Node.valueGroupId id

    let dataPoints = (\(a,b) -> Vector2 (fromIntegral a) (fromIntegral b)) <$> v
        widget = ScatterPlot.create Style.plotSize
               & ScatterPlot.dataPoints .~ dataPoints
               & ScatterPlot.display    .~ ScatterPlot.Bars
    UICmd.register_ groupId widget def

visualizeNodeValue id (Image url w h) = do
    groupId <- Node.valueGroupId id

    let widget = Image.create (Vector2 w h) $ Text.pack url
    UICmd.register_ groupId widget def

visualizeNodeValue id (StringValue str) = do
    groupId <- Node.valueGroupId id

    let normalize = intercalate "<br />" . wordsBy (== '\n')
    let widget = LongText.create (Vector2 200 200) (Text.pack $ normalize str) LongText.Left
    UICmd.register_ groupId widget def

visualizeNodeValue id (DataFrame cols) = do
    groupId <- Node.valueGroupId id

    let heads = Text.pack <$> fst <$> cols
        cols' = (fmap DefaultValue.stringify) <$> snd <$> cols
        rows = transpose cols'
        df = DataFrame.create (Vector2 400 200) heads rows
    UICmd.register_ groupId df def



-- visualizeNodeValue id (IntValue v) = do -- For Image widget testing
--     groupId <- Node.valueGroupId id
--
--     let widget = Image.create (Vector2 353 269) $ ""
--     UICmd.register_ groupId widget def

visualizeNodeValue _ _ = return ()


visualizeError :: NodeId -> String -> Command UIRegistry.State ()
visualizeError id str = do
    groupId <- Node.valueGroupId id
    let widget = LongText.create (Vector2 200 200) (Text.pack str) LongText.Left
    UICmd.register_ groupId widget def

updateNodeValue :: NodeId -> NodeResult.NodeValue -> Command State ()
updateNodeValue id val = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    withJust widgetId $ \widgetId -> do
        removeVisualization widgetId
        case val of
            NodeResult.Value val -> do
                UICmd.update_ widgetId $ Model.value   .~ (nodeValueToText val)
                UICmd.update_ widgetId $ Model.isError .~ False
                visualizeNodeValue widgetId val
            NodeResult.NoValue -> do
                UICmd.update_ widgetId $ Model.value   .~ ""
                UICmd.update_ widgetId $ Model.isError .~ False
            NodeResult.Error msg -> do
                UICmd.update_ widgetId $ Model.value   .~ "Error!"
                UICmd.update_ widgetId $ Model.isError .~ True
                visualizeError widgetId $ show msg

updateNodeProfilingData :: NodeId -> Integer -> Command State ()
updateNodeProfilingData id execTime = inRegistry $ do
    widgetId <- nodeIdToWidgetId id
    withJust widgetId $ flip UICmd.update_ $ Model.execTime ?~ execTime
