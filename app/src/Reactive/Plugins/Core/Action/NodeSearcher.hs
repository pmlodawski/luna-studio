{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Prelude       hiding       ( mapM_, forM_ )
import           Data.Foldable              ( mapM_, forM_ )
import           Control.Lens
import           Control.Applicative
import           Data.Default
import           Data.Maybe
import           Data.List
import           Data.Monoid
import           Data.Function
import           Data.Dynamic
import           System.Mem

import           JS.Bindings
import           GHCJS.Foreign
import           JS.Appjs
import qualified JS.NodeSearcher

import           Object.Object
import qualified Object.Node    as Node     ( position )
import           Object.Node    hiding      ( position )
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.NodeSearcher hiding      ( Event )
import qualified Event.NodeSearcher as NodeSearcher
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Utils.Wrapper
import           Utils.PrettyPrinter
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global    as Global
import qualified Reactive.Plugins.Core.Action.State.NodeSearcher    as NodeSearcherState

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)



data Action = Query      { _expression :: Text }
            | CreateNode { _expression :: Text }
            | OpenNodeSearcher
            -- | CloseNodeSearcher
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display (Query expr)        = "ns(query [" ++ (Text.unpack expr) ++ "])"
    display (CreateNode expr)   = "ns(create [" ++ (Text.unpack expr) ++ "])"
    display OpenNodeSearcher    = "ns(open )"


toAction :: Event Node -> Maybe Action
toAction (NodeSearcher (NodeSearcher.Event tpe expr)) = case tpe of
    "query"  -> Just $ Query expr
    "create" -> Just $ CreateNode expr
    _        -> Nothing

toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\t'   -> Just $ OpenNodeSearcher
    _      -> Nothing

toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction oldState
        -- where
        -- oldNodeSearcher                  = oldState ^. Global.nodeSearcher . NodeSearcherState.isOpen
        -- newState                         = oldState ^. Global.nodeSearcher .~ newNodeSearcher
        -- newNodeSearcher                  = case newAction of
        --     (Just OpenNodeSearcher)   -> oldNodeSearcher & NodeSearcherState.isOpen .~ True
        --     (Just CloseNodeSearcher)  -> oldNodeSearcher & NodeSearcherState.isOpen .~ True
        -- newAction                        =  newActionCandidate

mockQueryResults :: [JS.NodeSearcher.QueryResult]
mockQueryResults =
    [
     (JS.NodeSearcher.QueryResult "Math" "sqrt" "Math.sqrt" [JS.NodeSearcher.Highlight 1 3] "function"),
     (JS.NodeSearcher.QueryResult "Math" "Trig" "Math.Trig" [JS.NodeSearcher.Highlight 1 2] "module"),
     (JS.NodeSearcher.QueryResult "Math.Trig" "sin" "Math.Trig.sin" [JS.NodeSearcher.Highlight 2 3] "function")
    ]

instance ActionUIUpdater Action where
    updatUI (WithState action state) = case action of
        (Query expr)            -> do
            JS.NodeSearcher.displayQueryResults mockQueryResults
            putStrLn $ display action
        (CreateNode expr)       -> do
            putStrLn $ display action
        OpenNodeSearcher        -> do
            putStrLn $ display action
            JS.NodeSearcher.initNodeSearcher "currentNodeExpressionOrEmpty" (state ^. Global.mousePos . x) (state ^. Global.mousePos . y)
        -- CloseNodeSearcher      -> do
        --     putStrLn $ display action
        --     JS.NodeSearcher.destroyNodeSearcher
