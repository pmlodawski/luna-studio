{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Reactive.Plugins.Core.Action.Tutorial where

import           Control.Monad.Trans.Maybe         (runMaybeT)
import qualified Data.Text.Lazy                    as Text
import           Utils.PreludePlus                 hiding (Choice)
import           Utils.Vector

import qualified Empire.API.Data.DefaultValue      as DefaultValue
import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.Port              as Port
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Event.Batch                       as Batch
import qualified Event.CustomEvent                 as CustomEvent
import           Event.Event                       (Event (..))
import           Event.Keyboard                    (KeyMods (..), shift)
import qualified Event.Keyboard                    as Keyboard
import qualified Event.NodeSearcher                as NodeSearcher

import qualified Batch.Workspace                   as Workspace
import           Object.Widget                     (widget)
import qualified Object.Widget.Node                as UINode
import           Reactive.Commands.Command         (Command, performIO)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph

import           JS.Tutorial                       (showStep, closeOnboarding)


--  0. TAB
--  1. enter expression: readFile "/userdata/why_fp_matters.txt"
--  2. expand node to inspect details and results
--  3. press tab while first node is selected and add "length node"
--  4. select readFile node && tab: words  -- tu warto dodac wczesniej, ze przesun node length wyzej?
--  5. tab -> map _.length
--  6. tab -> sort
--  7. tab -> histogram
--  8. unselect node, add "switch False" -- docelowo bedzie samo switch
--  9. unselect node, add "/userdata/why_fp_matters.txt" node
-- 10. connect why_fp_matters to second port of switch node
-- 11. connect cakeipsum to third port of switch
-- 12. connect switch to first port of readFile
-- 13. expand switch node and change first argument


toAction :: Event -> Maybe (Command Global.State ())
-- press tab to open node searcher
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   mods@(KeyMods { _shift  = False })))    = Just $ whenStep 0  $ nextStep
-- enter expression readFile "/userdata/why_fp_matters.txt"
toAction (NodeSearcher (NodeSearcher.Create "readFile \"/userdata/why_fp_matters.txt\"" Nothing)) = Just $ whenStep 1  $ andNothingIsSelected     $ nextStep
-- expand switch node
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\r'  _))                                      = Just $ do
    whenStep 2  $ andIsSelected "readFile" $ nextStep
    whenStep 13 $ andIsSelected "switch"   $ nextStep
-- tab > "length"
toAction (NodeSearcher (NodeSearcher.Create "length" Nothing))                                    = Just $ whenStep 3  $ andIsSelected "readFile" $ nextStep
-- select "readFile" node and tab > "words"
toAction (NodeSearcher (NodeSearcher.Create "words" Nothing))                                     = Just $ whenStep 4  $ andIsSelected "readFile" $ nextStep
-- tab > "map _.length"
toAction (NodeSearcher (NodeSearcher.Create "map _.length" Nothing))                              = Just $ whenStep 5  $ andIsSelected "words"    $ nextStep
-- tab > sort
toAction (NodeSearcher (NodeSearcher.Create "sort" Nothing))                                      = Just $ whenStep 6  $ andIsSelected "map"      $ nextStep
-- tab > histogram
toAction (NodeSearcher (NodeSearcher.Create "histogram" Nothing))                                 = Just $ whenStep 7  $ andIsSelected "sort"     $ nextStep
-- tab > switch
toAction (NodeSearcher (NodeSearcher.Create (Text.stripPrefix "switch" -> Just _) Nothing))                                    = Just $ whenStep 8  $ andNothingIsSelected     $ nextStep
-- tab > switch
toAction (NodeSearcher (NodeSearcher.Create "\"/userdata/why_fp_matters.txt\"" Nothing))          = Just $ whenStep 9  $ andNothingIsSelected     $ nextStep
toAction (NodeSearcher (NodeSearcher.Create "\"/userdata/cakeipsum.txt\"" Nothing))               = Just $ whenStep 10 $ andNothingIsSelected     $ nextStep
-- connect switch to readFile
toAction (Batch        (Batch.NodesConnected update))                                             = Just $ do
    shouldProcess <- isCurrentLocation (update ^. Connect.location')
    when shouldProcess $ do
        whenStep 11 $ andConnected update "\"/userdata/why_fp_matters.txt\"" "switch"   (Port.Arg 1) $ nextStep
        whenStep 12 $ andConnected update "\"/userdata/cakeipsum.txt\""      "switch"   (Port.Arg 2) $ nextStep
        whenStep 14 $ andConnected update "switch"                           "readFile" (Port.Arg 0) $ nextStep

toAction (Batch        (Batch.NodeUpdated update))                                                = Just $ do
    shouldProcess <- isCurrentLocation (update ^. NodeUpdate.location)
    when shouldProcess $ do
        whenStep 15 $ andPortDefaultChanged update "switch" (Port.Arg 0) (DefaultValue.BoolValue True) $ nextStep
toAction (CustomEvent (CustomEvent.RawEvent "closeOnboarding" _)) = Just $ do
    Global.tutorial .= Nothing
    performIO closeOnboarding
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
                    isPrefixOk = Text.isPrefixOf prefix expr
                when isPrefixOk action
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

        when ((Text.isInfixOf expr1 srcExpr) && (Text.isInfixOf expr2 dstExpr)) $ lift action

andPortDefaultChanged :: NodeUpdate.Update -> Text -> Port.InPort -> DefaultValue.Value -> Command Global.State () -> Command Global.State ()
andPortDefaultChanged update expr portId value action = do
    let node     = update ^. NodeUpdate.node
        nodeExpr = node ^? Node.nodeType . Node._ExpressionNode

    withJust nodeExpr $ \nodeExpr -> do
        let isExprOk = Text.isInfixOf expr nodeExpr

        when isExprOk $ do
            let portDefault = node ^? Node.ports . ix (Port.InPortId portId) . Port.state . Port._WithDefault . DefaultValue._Constant
            when (portDefault == (Just value)) nextStep

isCurrentLocation :: GraphLocation -> Command Global.State Bool
isCurrentLocation location = uses (Global.workspace . Workspace.currentLocation) (== location)
