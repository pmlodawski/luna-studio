{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Reactive.Plugins.Core.Executor where


import           Utils.PreludePlus

import           Reactive.Banana
import           Reactive.Banana.Frameworks

import           Reactive.Plugins.Core.Action
import           Reactive.Plugins.Core.Action.State.Global



execAll :: Behavior t State -> [Behavior t ActionST] -> (Behavior t State, [Behavior t ActionUI])
execAll stInitB actionBs = (getState <$> (last result), tail result) where
    result = execAll' (noActionUI <$> stInitB) actionBs

execAll' :: Behavior t ActionUI -> [Behavior t ActionST] -> [Behavior t ActionUI]
execAll' initB actionBs = scanl exec initB actionBs where
    exec = liftA2 exec'
    exec' (ActionUI _ st) (ActionST act) = execSt act st
