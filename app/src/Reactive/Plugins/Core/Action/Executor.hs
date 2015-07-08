{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Reactive.Plugins.Core.Action.Executor where

import           Data.Monoid          ( (<>) )
import           Data.Default
import           Data.Maybe
import           Data.Functor
import           Control.Lens
import           Utils.PrettyPrinter

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.Utils
import           Reactive.Handlers

import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.AddRemove as AddRemove
import qualified Reactive.Plugins.Core.Action.Selection as Selection
import qualified Reactive.Plugins.Core.Action.Drag      as Drag
import qualified Reactive.Plugins.Core.Action.Camera    as Camera
import qualified Reactive.Plugins.Core.Action.NodeSearcher as NodeSearcher
import           Reactive.Plugins.Core.Action.State.Global



execAll :: Behavior t State -> [Behavior t ActionST] -> [Behavior t ActionUI]
execAll stInitB actionBs = execAll' (noActionUI <$> stInitB) actionBs

execAll' :: Behavior t ActionUI -> [Behavior t ActionST] -> [Behavior t ActionUI]
execAll' initB actionBs = scanl exec initB actionBs
    where
    exec :: Behavior t ActionUI -> Behavior t ActionST -> Behavior t ActionUI
    exec pB cB = exec' <$> pB <*> cB
        where
        exec' :: ActionUI -> ActionST -> ActionUI
        exec' (ActionUI _ st) (ActionST act) = execSt act st
