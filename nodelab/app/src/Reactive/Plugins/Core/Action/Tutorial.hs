{-# LANGUAGE OverloadedStrings #-}
module Reactive.Plugins.Core.Action.Tutorial where

import           Control.Monad.Trans.Maybe         (runMaybeT)
import qualified Data.Text.Lazy                    as Text
import           Utils.PreludePlus                 hiding (Choice)
import           Utils.Vector

import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.Port              as Port
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Event.Batch                       as Batch
import           Event.Event                       (Event (..))
import           Event.Keyboard                    (KeyMods (..), shift)
import qualified Event.Keyboard                    as Keyboard
import qualified Event.NodeSearcher                as NodeSearcher
import           Empire.API.Data.GraphLocation               (GraphLocation)

import qualified Batch.Workspace                   as Workspace
import           Object.Widget                     (widget)
import qualified Object.Widget.Node                as UINode
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph

import           JS.Tutorial                       (showStep)


toAction :: Event -> Maybe (Command Global.State ())
-- press tab to open node searcher
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   mods@(KeyMods { _shift  = False })))    = Just $ whenStep 0 $ nextStep
-- enter expression readFile "/userdata/why_fp_matters.txt"
toAction (NodeSearcher (NodeSearcher.Create "readFile \"/userdata/why_fp_matters.txt\"" Nothing)) = Just $ whenStep 1 $ andNothingIsSelected $ nextStep
-- expand switch node
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\r'  _))                                      = Just $ do
    whenStep 2 $ andIsSelected "readFile" $ nextStep
    whenStep 13 $ andIsSelected "switch"   $ nextStep
-- tab > "length"
toAction (NodeSearcher (NodeSearcher.Create "length" Nothing))                                    = Just $ whenStep 3 $ andIsSelected "readFile" $ nextStep
-- select "readFile" node and tab > "words"
toAction (NodeSearcher (NodeSearcher.Create "words" Nothing))                                     = Just $ whenStep 4 $ andIsSelected "readFile" $ nextStep
-- tab > "map _.length"
toAction (NodeSearcher (NodeSearcher.Create "map _.length" Nothing))                              = Just $ whenStep 5 $ andIsSelected "words" $ nextStep
-- tab > sort
toAction (NodeSearcher (NodeSearcher.Create "sort" Nothing))                                      = Just $ whenStep 6 $ andIsSelected "map" $ nextStep
-- tab > histogram
toAction (NodeSearcher (NodeSearcher.Create "histogram" Nothing))                                 = Just $ whenStep 7 $ andIsSelected "sort" $ nextStep
-- tab > switch
toAction (NodeSearcher (NodeSearcher.Create "switch" Nothing))                                    = Just $ whenStep 8 $ andNothingIsSelected $ nextStep
-- tab > switch
toAction (NodeSearcher (NodeSearcher.Create "\"/userdata/why_fp_matters.txt\"" Nothing))          = Just $ whenStep 9 $ andNothingIsSelected $ nextStep
toAction (NodeSearcher (NodeSearcher.Create "\"/userdata/cakeipsum.txt\"" Nothing))               = Just $ whenStep 10 $ andNothingIsSelected $ nextStep
-- connect switch to readFile
toAction (Batch        (Batch.NodesConnected update))                                             = Just $ do
    shouldProcess <- isCurrentLocation (update ^. Connect.location')
    when shouldProcess $ do
        whenStep 11 $ andConnected update "\"/userdata/why_fp_matters.txt\"" "switch"   (Port.Arg 1) $ nextStep
        whenStep 12 $ andConnected update "\"/userdata/cakeipsum.txt\""      "switch"   (Port.Arg 2) $ nextStep
        whenStep 14 $ andConnected update "switch"                           "readFile" (Port.Arg 0) $ nextStep

-- toAction (Batch        (Batch.NodeUpdated update))                                                = Just $ do
--     shouldProcess <- isCurrentLocationAndGraphLoaded (update ^. NodeUpdate.location)
--     when shouldProcess $ do
--         whenStep 9 $ andConnected update "switch" "readFile" $ nextStep

toAction _  = Nothing



whenStep :: Int -> Command Global.State () -> Command Global.State ()
whenStep ix action = do
    currentStep <- use Global.tutorial
    when (currentStep == (Just ix)) action

goToStep :: Int -> Command Global.State ()
goToStep ix = do
    Global.tutorial ?= ix
    performIO $ showStep ix

nextStep :: Command Global.State ()
nextStep = do
    currentStep <- use Global.tutorial
    withJust currentStep $ \currentStep -> goToStep (currentStep + 1)

andIsSelected :: Text -> Command Global.State () -> Command Global.State ()
andIsSelected prefix action = do
    selected   <- selectedNodes
    case selected of
            [wf]   -> do
                let expr = wf ^. widget . UINode.expression
                    isPrefixOk = Text.stripPrefix prefix expr
                when (isJust isPrefixOk) action
            []     -> return ()
            (_:_)  -> return ()

andNothingIsSelected :: Command Global.State () -> Command Global.State ()
andNothingIsSelected action = do
    selected   <- selectedNodes
    when (length selected == 0) action

andConnected :: Connect.Update -> Text -> Text -> Port.InPort -> Command Global.State () -> Command Global.State ()
andConnected update expr1 expr2 portId action = void $ runMaybeT $ do
    let src = update ^. Connect.src' . PortRef.srcNodeId
        dst = update ^. Connect.dst' . PortRef.dstNodeId
        dstPortId = update ^. Connect.dst' . PortRef.dstPortId
    when (portId == dstPortId) $ do
        (Just srcExpr) <- preuse $ Global.graph . Graph.nodesMap . ix src . Node.nodeType . Node._ExpressionNode
        (Just dstExpr) <- preuse $ Global.graph . Graph.nodesMap . ix dst . Node.nodeType . Node._ExpressionNode

        when ((isJust $ Text.stripPrefix expr1 srcExpr) && (isJust $ Text.stripPrefix expr2 dstExpr)) $ lift action

isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)
