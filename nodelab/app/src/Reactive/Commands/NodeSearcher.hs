{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.NodeSearcher where


import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import qualified Data.Text.Lazy                                  as Text
import           Utils.PreludePlus
import           Utils.Vector

import qualified JS.NodeSearcher                                 as UI

import qualified Batch.Workspace                                 as Workspace
import qualified Event.Keyboard                                  as Keyboard
import qualified Event.NodeSearcher                              as NodeSearcher
import qualified Object.Node                                     as Node
import qualified Object.Widget                                   as Widget
import qualified Object.Widget.Node                              as NodeModel

import           Reactive.Commands.Command                       (Command, performIO)
import           Reactive.Commands.RegisterNode                  (registerNode)
import           Reactive.Commands.Selection                     (selectedNodes)
import           Reactive.Commands.Camera                        (syncCamera)
import           Reactive.Commands.UpdateNode                    ()
import           Reactive.State.Global                           (inRegistry)
import qualified Reactive.State.Global                           as Global
import qualified Reactive.State.Graph                            as Graph
import qualified Reactive.State.UIElements                       as UIElements
import qualified Reactive.State.Camera                           as Camera
import qualified Reactive.State.UIRegistry                       as UIRegistry

import qualified Empire.API.Data.Node                            as Node
import           Empire.API.Data.NodeSearcher                    (Item (..), ModuleItems, _Module)
import qualified Empire.API.Data.Port                            as Port
import qualified Empire.API.Data.ValueType                       as ValueType
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Scope as Scope

searcherData :: Command Global.State ModuleItems
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

openFresh :: Command Global.State ()
openFresh = do
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    let offset = Vector2 0 (floor $ -40.0 * factor)
    (nsPos', nsPos) <- ensureNSVisible
    Global.uiElements . UIElements.nsPos .= nsPos'
    performIO $ UI.initNodeSearcher "" 0 (nsPos + offset) False

position :: Command Global.State (Vector2 Double, Vector2 Int)
position = do
    mousePos <- use Global.mousePos
    mousePos' <- zoom Global.camera $ Camera.screenToWorkspaceM mousePos
    factor <- use $ Global.camera . Camera.camera . Camera.factor
    selected <- inRegistry selectedNodes
    nsPos <- zoom Global.camera $ Camera.workspaceToScreen $ case selected of
            [wf]   -> (wf ^. Widget.widget . NodeModel.position) + (Vector2 230.0 0)
            _     -> mousePos'
    nsPos' <- zoom Global.camera $ Camera.screenToWorkspaceM nsPos
    return (nsPos', nsPos)

ensureNSVisible :: Command Global.State (Vector2 Double, Vector2 Int)
ensureNSVisible = do
    (workspacePos, screenPos) <- position
    screenSize <- use $ Global.camera . Camera.camera . Camera.screenSize
    x' <- case (screenPos ^. x > (screenSize ^. x - 250)) of
        True -> do
            Global.camera . Camera.camera . Camera.pan . x .= workspacePos ^. x
            zoom Global.camera syncCamera
            return (floor $ (fromIntegral $ screenSize ^. x) / 2.0)
        False -> return $ screenPos ^. x
    y' <- case (screenPos ^. y > (screenSize ^. y - 250)) of
        True -> do
            Global.camera . Camera.camera . Camera.pan . y .= workspacePos ^. y
            zoom Global.camera syncCamera
            return (floor $ (fromIntegral $ screenSize ^. y) / 2.0)
        False -> return $ screenPos ^. y

    return $ (workspacePos, Vector2 x' y')

globalFunctions :: ModuleItems -> ModuleItems
globalFunctions items = Map.filter (== Function) items

scopedData :: Command Global.State ModuleItems
scopedData = do
    completeData <- searcherData
    selected   <- inRegistry selectedNodes
    scope <- case selected of
            []     -> return Nothing
            [wf]   -> do
                let nodeId = wf ^. Widget.widget . NodeModel.nodeId
                vt <- preuse $ Global.graph . Graph.nodes . ix nodeId . Node.ports . ix (Port.OutPortId Port.All) . Port.valueType
                return $ case vt of
                    Nothing -> Nothing
                    Just vt -> case vt of
                        ValueType.AnyType -> Nothing
                        ValueType.TypeIdent ti -> Just ti
            (_:_) -> return Nothing
    performIO $ putStrLn $ show scope
    case scope of
        Nothing -> return completeData
        Just tn -> do
            let gf = globalFunctions completeData
                items = completeData
                tn' = if Text.isPrefixOf "[" (Text.pack tn) then "List" else (Text.pack tn)
                mayScope = items ^? ix tn' . _Module
                scope = fromMaybe mempty mayScope
                scopefuns = globalFunctions scope
                overallScope = Map.union scopefuns gf
            performIO $ putStrLn $ show overallScope
            return overallScope


querySearch :: Text -> Command Global.State ()
querySearch query = do
    sd <- scopedData
    let items = Scope.searchInScope False sd query
    performIO $ UI.displayQueryResults items

queryTree :: Text -> Command Global.State ()
queryTree query = do
    sd <- scopedData
    let items = Scope.moduleItems False sd query
    performIO $ UI.displayTreeResults items

openCommand :: Command Global.State ()
openCommand = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos True
