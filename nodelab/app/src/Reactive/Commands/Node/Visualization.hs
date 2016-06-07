{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Visualization
    ( removeVisualization
    , visualizeNodeValue
    , visualizeError
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Control.Monad.State               hiding (State)
import           Data.Hashable                     (hash)
import           Data.List                         (intercalate)
import           Data.List.Split                   (wordsBy)
import qualified Data.Map.Lazy                     as Map
import qualified Data.Text.Lazy                    as Text
import           GHC.Float                         (double2Float)

import           Object.UITypes                    (WidgetId)
import           Object.Widget                     (objectId, widget)
import qualified Object.Widget.Button              as Button
import qualified Object.Widget.DataFrame           as DataFrame
import qualified Object.Widget.Group               as Group
import qualified Object.Widget.Label               as Label
import           Object.Widget.LabeledTextBox      (LabeledTextBox (..))
import qualified Object.Widget.LabeledTextBox      as LabeledTextBox
import qualified Object.Widget.LongText            as LongText
import qualified Object.Widget.Node                as Model
import           Object.Widget.Number.Continuous   (ContinuousNumber (..))
import qualified Object.Widget.Number.Continuous   as ContinuousNumber
import           Object.Widget.Number.Discrete     (DiscreteNumber (..))
import qualified Object.Widget.Number.Discrete     as DiscreteNumber
import qualified Object.Widget.Plots.Image         as Image
import qualified Object.Widget.Plots.ScatterPlot   as ScatterPlot
import qualified Object.Widget.Port                as PortModel
import           Object.Widget.Toggle              (Toggle (..))
import qualified Object.Widget.Toggle              as Toggle
import qualified UI.Handlers.Button                as Button
import           UI.Handlers.Generic               (onValueChanged)
import qualified UI.Handlers.LabeledTextBox        as LabeledTextBox
import qualified UI.Handlers.Node                  as Node
import qualified UI.Handlers.Number.Continuous     as ContinuousNumber
import qualified UI.Handlers.Number.Discrete       as DiscreteNumber
import qualified UI.Handlers.Toggle                as Toggle

import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.EnterNode       (enterNode)
import           Reactive.Commands.Graph           (colorPort, focusNode, nodeIdToWidgetId, portDefaultAngle,
                                                    updateConnections, updateNodeMeta)
-- import           Reactive.Commands.PendingNode   (unrenderPending)
import           Reactive.Commands.RemoveNode      (removeSelectedNodes)
import           Reactive.Commands.Selection       (selectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.UIRegistry         (addHandler, sceneGraphId)
import qualified Reactive.State.UIRegistry         as UIRegistry

import qualified Reactive.Commands.Batch           as BatchCmd
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
import qualified Empire.API.Data.Error             as LunaError
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.Port              (InPort (..), InPort (..), OutPort (..), Port (..), PortId (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (AnyPortRef (..), InPortRef (..), toAnyPortRef)
import           Empire.API.Data.TypeRep           (TypeRep)
import           Empire.API.Data.ValueType         (ValueType (..))
import qualified Empire.API.Data.ValueType         as ValueType
import qualified Empire.API.Graph.NodeResultUpdate as NodeResult

import qualified Object.Widget.Graphics            as G
import qualified Utils.Shader                      as Shader
import qualified Graphics.API                      as GR


zipVector :: [Double] -> [Vector2 Double] -> [Vector2 Double]
zipVector (ax:xs) [] = zipVector xs [Vector2 0 ax]
zipVector (ax:xs) (acc:accs) = zipVector xs ((Vector2 (acc ^. x + 1.0) ax):acc:accs)
zipVector [] accs = accs

zipVectorInt :: [Int] -> [Vector2 Double] -> [Vector2 Double]
zipVectorInt (ax:xs) [] = zipVectorInt xs [Vector2 0 (fromIntegral ax)]
zipVectorInt (ax:xs) (acc:accs) = zipVectorInt xs ((Vector2 (acc ^. x + 1.0) (fromIntegral ax)):acc:accs)
zipVectorInt [] accs = accs


visualizeError :: WidgetId -> LunaError.Error TypeRep -> Command UIRegistry.State ()
visualizeError id err = do
    groupId <- Node.valueGroupId id
    let msg  = case err of
            LunaError.ImportError   name     -> "Cannot find symbol \"" <> name        <> "\""
            LunaError.NoMethodError name tpe -> "Cannot find method \"" <> name        <> "\" for type \"" <> toString tpe <> "\""
            LunaError.TypeError     t1   t2  -> "Cannot match type  \"" <> toString t1 <> "\" with \""     <> toString t2  <> "\""
        widget = LongText.create (Vector2 200 200) (Text.pack msg) LongText.Left
    UICmd.register_ groupId widget def



removeVisualization :: WidgetId -> Command UIRegistry.State ()
removeVisualization id = do
    groupId <- Node.valueGroupId id
    widgets <- UICmd.children groupId
    forM_ widgets UICmd.removeWidget


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

visualizeNodeValue id (Graphics (GR.Graphics layers)) = do
    groupId <- Node.valueGroupId id
    let items = createItem <$> layers
    let widget = G.create (Vector2 200 200) items
    UICmd.register_ groupId widget def
    where
        createItem (GR.Layer shader trans) = G.Item (Text.pack shaderTxt) boxes where
            (shaderTxt, (w, h)) = Shader.createShader shader
            boxes               = createBox <$> trans
            createBox (GR.Transformation sx sy dx dy rot refl) = G.Box (Vector2 dx dy) (Vector2 w h)

visualizeNodeValue _ _ = return ()
