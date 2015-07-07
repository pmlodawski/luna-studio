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

instance ActionStateExecutor Action Global.State where
    exec newAction oldState = WithState (Just newAction) oldState
        -- where
        -- oldNodeSearcher                  = oldState ^. NodeSearcher.isOpen
        -- newState                         = oldState & Global.nodeSearcher .~ newNodeSearcher
        -- newNodeSearcher                  = case newAction of
        --     OpenNodeSearcher   -> oldNodeSearcher & NodeSearcher.isOpen .~ True
        --     CloseNodeSearcher  -> oldNodeSearcher & NodeSearcher.isOpen .~ True


updateUI :: WithStateMaybe Action Global.State -> IO ()
updateUI (WithState maybeAction state) = case maybeAction of
    Nothing         -> return ()
    Just action     -> case action of
        (Query expr)            -> do
            putStrLn $ display action
        (CreateNode expr)       -> do
            putStrLn $ display action
        OpenNodeSearcher        -> do
            putStrLn $ display action
            JS.NodeSearcher.initNodeSearcher "currentNodeExpressionOrEmpty" (state ^. Global.mousePos . x) (state ^. Global.mousePos . y)
        -- CloseNodeSearcher      -> do
        --     putStrLn $ display action
        --     JS.NodeSearcher.destroyNodeSearcher
