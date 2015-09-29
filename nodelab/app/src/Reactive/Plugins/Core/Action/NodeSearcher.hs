{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Foreign

import           JS.Bindings
import qualified JS.NodeGraph   as UI
import qualified JS.NodeSearcher

import           Object.Object
import           Object.Node
import           Event.Keyboard hiding      ( Event )
import qualified Event.Keyboard as Keyboard
import           Event.NodeSearcher hiding  ( Event )
import qualified Event.NodeSearcher as NodeSearcher
import           Event.Mouse    hiding      ( Event )
import qualified Event.Mouse    as Mouse
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.NodeSearcher
import qualified Reactive.Plugins.Core.Action.State.Global          as Global
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Mock     as Mock

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)


data QueryType = Search | Tree deriving (Eq, Show)

data Action = Query      { _tpe        :: QueryType
                         , _expression :: Text
                         }
            | CreateNode { _expression :: Text }
            | OpenNodeSearcher
            -- | CloseNodeSearcher
            deriving (Eq, Show)


makeLenses ''Action

instance PrettyPrinter Action where
    display (Query Search expr) = "ns(query ["     ++ (Text.unpack expr) ++ "])"
    display (Query Tree   expr) = "ns(treequery [" ++ (Text.unpack expr) ++ "])"
    display (CreateNode   expr) = "ns(create ["    ++ (Text.unpack expr) ++ "])"
    display OpenNodeSearcher    = "ns(open )"


toAction :: Event Node -> Maybe Action
toAction (NodeSearcher (NodeSearcher.Event tpe expr))   = case tpe of
    "query"  -> Just $ Query Search expr
    "tree"   -> Just $ Query Tree   expr
    _        -> Nothing

toAction (Keyboard (Keyboard.Event Keyboard.Down char)) = case char of
    '\t'   -> Just $ OpenNodeSearcher
    _      -> Nothing

toAction _ = Nothing

instance ActionStateUpdater Action where
    execSt newAction oldState = ActionUI newAction oldState
        -- where
        -- oldNodeSearcher                  = oldState ^. Global.nodeSearcher . isOpen
        -- newState                         = oldState ^. Global.nodeSearcher .~ newNodeSearcher
        -- newNodeSearcher                  = case newAction of
        --     (Just OpenNodeSearcher)   -> oldNodeSearcher & isOpen .~ True
        --     (Just CloseNodeSearcher)  -> oldNodeSearcher & isOpen .~ True
        -- newAction                        =  newActionCandidate



instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        (Query Search expr)      -> do
            JS.NodeSearcher.displayQueryResults $ Mock.getItemsSearch expr
        (Query Tree expr)        -> do
            JS.NodeSearcher.displayTreeResults $ Mock.getItemsTree expr
        OpenNodeSearcher         -> do
            JS.NodeSearcher.initNodeSearcher "" (state ^. Global.mousePos . x) (state ^. Global.mousePos . y)
        -- CloseNodeSearcher      -> do
        --     putStrLn $ display action
        --     JS.NodeSearcher.destroyNodeSearcher


