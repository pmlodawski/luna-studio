{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Visualization
    ( removeVisualization
    , visualizeNodeValue
    , visualizeError
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Data.List.Split                 (wordsBy)
import qualified Data.Text.Lazy                  as Text

import           Object.UITypes                  (WidgetId)
import           Object.Widget                   (DisplayObjectClass, CompositeWidget)
import qualified Object.Widget.DataFrame         as DataFrame
import qualified Object.Widget.Graphics          as Graphics
import qualified Object.Widget.LongText          as LongText
import qualified Object.Widget.Plots.Image       as Image
import qualified Object.Widget.Plots.ScatterPlot as ScatterPlot
import qualified UI.Handlers.Node                as Node
import qualified UI.Instances                    ()

import           Reactive.Commands.Command       (Command)
import qualified Reactive.Commands.UIRegistry    as UICmd
import qualified Reactive.State.UIRegistry       as UIRegistry

import qualified Style.Node                      as Style

import           Empire.API.Data.DefaultValue    (Value (..))
import qualified Empire.API.Data.DefaultValue    as DefaultValue
import qualified Empire.API.Data.Error           as LunaError
import           Empire.API.Data.TypeRep         (TypeRep)

import qualified Graphics.API                    as GR
import qualified Utils.Shader                    as Shader


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
    void $ mapM UICmd.removeWidget widgets


displayListTable :: WidgetId -> [Text] -> Command UIRegistry.State ()
displayListTable groupId col = do
    let idxs = Text.pack . show <$> take (length col) [1..]
        cols = [idxs, col]
        rows = transpose cols
        df = DataFrame.create Style.plotSize ["Index", "Value"] rows
    UICmd.register_ groupId df def

visualize :: (Eq a, CompositeWidget a, DisplayObjectClass a) => WidgetId -> (WidgetId -> Command UIRegistry.State ()) -> (a -> a) -> Command UIRegistry.State ()
visualize id create update = do
    groupId <- Node.valueGroupId id
    currentVisualizations <- UICmd.children groupId
    let currentVisualization = listToMaybe currentVisualizations
        cleanup = do
            widgets <- UICmd.children groupId
            void $ mapM UICmd.removeWidget widgets

    case currentVisualization of
        Nothing -> do
            cleanup
            create groupId
        Just currentVis -> do
            status <- UICmd.tryUpdate id update
            when (status == False) $ create groupId


visualizeNodeValue :: WidgetId -> Value -> Command UIRegistry.State ()
visualizeNodeValue id (StringList v) = do
    groupId <- Node.valueGroupId id
    displayListTable groupId $ Text.pack . show <$> v

visualizeNodeValue id (IntList v) = do
    let dataPoints = zipVectorInt v []
    let create groupId = do
            let widget = ScatterPlot.create Style.plotSize
                       & ScatterPlot.dataPoints .~ dataPoints
            -- displayListTable groupId $ Text.pack . show <$> v
            UICmd.register_ groupId widget def
        update      = ScatterPlot.dataPoints .~ dataPoints
    visualize id create update

visualizeNodeValue id (DoubleList v) = do
    let dataPoints = zipVector v []
    let create groupId = do
            let widget = ScatterPlot.create Style.plotSize
                       & ScatterPlot.dataPoints .~ dataPoints
            -- displayListTable groupId $ Text.pack . show <$> v
            UICmd.register_ groupId widget def
        update      = ScatterPlot.dataPoints .~ dataPoints
    visualize id create update

visualizeNodeValue id (IntPairList v) = do
    let dataPoints = (\(a,b) -> Vector2 (fromIntegral a) (fromIntegral b)) <$> v
    let create groupId = do
            let widget = ScatterPlot.create Style.plotSize
                       & ScatterPlot.dataPoints .~ dataPoints
            UICmd.register_ groupId widget def
        update      = ScatterPlot.dataPoints .~ dataPoints
    visualize id create update

visualizeNodeValue id (DoublePairList v) = do
    let dataPoints = uncurry Vector2 <$> v
    let create groupId = do
            let widget = ScatterPlot.create Style.plotSize
                       & ScatterPlot.dataPoints .~ dataPoints
            UICmd.register_ groupId widget def
        update      = ScatterPlot.dataPoints .~ dataPoints
    visualize id create update

visualizeNodeValue id (Histogram v) = do
    let dataPoints = (\(a,b) -> Vector2 (fromIntegral a) (fromIntegral b)) <$> v
    let create groupId = do
            let widget = ScatterPlot.create Style.plotSize
                       & ScatterPlot.dataPoints .~ dataPoints
                       & ScatterPlot.display    .~ ScatterPlot.Bars
            UICmd.register_ groupId widget def
        update      = ScatterPlot.dataPoints .~ dataPoints
    visualize id create update

visualizeNodeValue id (Image url w h) = do
    let create groupId = do
            let widget = Image.create (Vector2 w h) $ Text.pack url
            UICmd.register_ groupId widget def
        update = (Image.size . x .~ w)
               . (Image.size . y .~ h)
               . (Image.image  .~ (Text.pack url))
    visualize id create update

visualizeNodeValue id (StringValue str) = do
    let normalize = intercalate "<br />" . wordsBy (== '\n')
        create groupId = do
            let widget = LongText.create (Vector2 200 200) (Text.pack $ normalize str) LongText.Left
            UICmd.register_ groupId widget def
        update = LongText.value .~ (Text.pack $ normalize str)
    visualize id create update

visualizeNodeValue id (DataFrame cols) = do
    let heads = Text.pack <$> fst <$> cols
        cols' = (fmap DefaultValue.stringify) <$> snd <$> cols
        rows = transpose cols'

    let create groupId = do
            let df = DataFrame.create (Vector2 400 200) heads rows
            UICmd.register_ groupId df def
        update      = (DataFrame.headers .~ heads)
                    . (DataFrame.rows    .~ rows )
    visualize id create update

visualizeNodeValue id (Graphics (GR.Graphics layers)) = do
    groupId <- Node.valueGroupId id
    let items  = createItem <$> layers
        labels = [] -- createLabel <$> layers
    let create groupId = do
            let widget = Graphics.create (Vector2 200 200) items labels
            UICmd.register_ groupId widget def
        update = (Graphics.items  .~ items )
               . (Graphics.labels .~ labels)
    visualize id create update
    where
        createItem (GR.Layer geometry trans) = Graphics.Item (Text.pack shaderTxt) boxes size offset where
            Shader.ShaderBox shaderTxt (Shader.Location size offset) = Shader.createShaderBox geometry
            boxes = createBox <$> trans
            createBox (GR.Transformation sx sy dx dy rot refl) = Graphics.Box (Vector2 dx dy)

visualizeNodeValue _ _ = return ()
