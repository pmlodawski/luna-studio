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



execAll :: State -> Maybe AddRemove.Action
                 -> Maybe Selection.Action
                 -> Maybe Drag.Action
                 -> Maybe Camera.Action
                 -> Maybe NodeSearcher.Action
                 -> ( State
                    , WithStateMaybe AddRemove.Action State
                    , WithStateMaybe Selection.Action State
                    , WithStateMaybe Drag.Action State
                    , WithStateMaybe Camera.Action State
                    , WithStateMaybe NodeSearcher.Action State
                    )
execAll initState addRem selection drag cam ns = (finalState, wsAddRem, wsSel, wsDrag, wsCam, wsNode)
    where
    wsAddRem   = tryExec addRem              initState
    wsSel      = tryExec selection $ wsAddRem ^. state
    wsDrag     = tryExec drag      $ wsSel    ^. state
    wsCam      = tryExec cam       $ wsDrag   ^. state
    wsNode     = tryExec ns        $ wsCam    ^. state
    finalState = wsCam                        ^. state


execAll' :: State -> Maybe AddRemove.Action
                 -> Maybe Selection.Action
                 -> Maybe Drag.Action
                 -> Maybe Camera.Action
                 -> Maybe NodeSearcher.Action
                 -> State

execAll' initState addRem selection drag cam ns = finalState
    where
    wsAddRem   = tryExec addRem              initState
    wsSel      = tryExec selection $ wsAddRem ^. state
    wsDrag     = tryExec drag      $ wsSel    ^. state
    wsCam      = tryExec cam       $ wsDrag   ^. state
    wsNS       = tryExec ns        $ wsCam    ^. state
    finalState = wsNS                         ^. state



class Foo a where
    foo :: a -> String

data FooLike = forall a. Foo a => FooLike a


instance Foo FooLike where
    foo (FooLike a) = foo a



-- data ActionExec = forall act. ActionStateExecutor act State => ActionExec act


-- instance ActionStateExecutor ActionExec State where
--     -- tryExec :: ActionExec -> State -> WithStateMaybe act State
--     exec (ActionExec a) st =
--         WithState (Just oo) outState
--         -- undefined
--         where
--             -- resultAP :: Int
--             result    = exec a st
--             outState  = result ^. state
--             outAction = result ^. action
--             oo  = case outAction of
--                 -- Nothing  -> ActionExec Nothing
--                 Just act -> ActionExec act


    -- exec (ActionExec a) st = exec a st

-- execAll3 :: forall act t. ActionStateExecutor act State
--          => Behavior t State -> [Behavior t ActionExec] -> Behavior t State


execAll3 :: Behavior t State -> [Behavior t ActionST] -> Behavior t State
execAll3 stInitB actionBs = foldl exec3 stInitB actionBs
    where
    exec3 :: Behavior t State -> Behavior t ActionST -> Behavior t State
    exec3 stB actB = fromMaybe <$> stB <*> run3
        where
        run3 = ((fmap . fmap) getState) $ execSt <$> actB <*> stB


-- execAll4 :: Behavior t State -> [Behavior t ActionST] -> Behavior t State
-- execAll4 stInitB actionBs = foldl exec3 stInitB actionBs
--     where
--     exec3 :: Behavior t State -> Behavior t ActionST -> Behavior t State
--     exec3 stB actB = fromMaybe <$> stB <*> run3
--         where
--         run3 = ((fmap . fmap) getState) $ execSt <$> actB <*> stB



    -- unpB :: forall a t. Behavior t ActionExec -> Behavior t (Maybe a)
    -- unpB beh = (fmap (\(ActionExec a) -> a)) beh
    -- where
    -- execAction :: WithStateMaybe act State -> Maybe act -> WithStateMaybe act State
    -- execAction ws act = tryExec act $ ws ^. state




-- class ActionStateExecutor act st where
--     exec    ::        act  -> st -> WithStateMaybe act st
--     tryExec :: (Maybe act) -> st -> WithStateMaybe act st
--     tryExec Nothing       = WithState Nothing
--     tryExec (Just action) = exec action



-- data ActionExecutorElem = forall act. ActionStateExecutor act State => ActionExecutorElem act

-- newtype ActionExecutorList = ActionExecutorList [ActionExecutorElem] deriving (Monoid)

-- single :: ActionStateExecutor act State => act -> ActionExecutorList
-- single act = ActionExecutorList [ActionExecutorElem act]

-- prepend :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- prepend act actionExecutorList = single act <> actionExecutorList

-- append :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- append act actionExecutorList = actionExecutorList <> single act

-- infixr 9 -:
-- (-:) :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- (-:) = append

-- data ActionExecutorElem = forall act. ActionStateExecutor act State => ActionExecutorElem act

-- newtype ActionExecutorList = ActionExecutorList [ActionExecutorElem] deriving (Monoid)

-- single :: ActionStateExecutor act State => act -> ActionExecutorList
-- single act = ActionExecutorList [ActionExecutorElem act]

-- prepend :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- prepend act actionExecutorList = single act <> actionExecutorList

-- append :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- append act actionExecutorList = actionExecutorList <> single act

-- infixr 9 -:
-- (-:) :: ActionStateExecutor act State => act -> ActionExecutorList -> ActionExecutorList
-- (-:) = append



-- toActionExecutorListB :: ActionStateExecutor act State => [Behavior t act] -> Behavior t ActionExecutorList
-- toActionExecutorListB list = undefined where
    -- a = catMaybes <$> list

-- toActionExecutorListB :: [Behavior t (forall act. ActionStateExecutor act State)]
--        -> ActionExecutorList (Behavior t forall act. ActionStateExecutor act State])
-- toActionExecutorListB list ->--






-- (Behavior t State -> Behavior t act -> Behavior t State) -> Behavior t State -> [Behavior  t act] -> Behavior t State

-- foldl        :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z0 xs0 = lgo z0 xs0
--              where
--                 lgo z []     =  z
--                 lgo z (x:xs) = lgo (f z x) xs

-- toActionExecutorListB :: forall act t. [Behavior t (ActionStateExecutor act State)]
--                       -> Behavior t ActionExecutorList ActionExecutorElem
-- toActionExecutorListB list = mconcat ((fmap ActionExecutorElem) <$> list)

-- infixr 9 :-:
-- data ActionExecList = forall a. ActionStateExecutor a State => a :-: ActionExecList
--                     | ActionExecNil

-- execAll2 :: forall act. ActionStateExecutor act State => State -> [Maybe act] -> [WithStateMaybe act State]
-- execAll2 stInit actions = scanl execAction (WithState Nothing stInit) actions
--     where
--     -- execAction :: WithStateMaybe act State -> Maybe act -> WithStateMaybe act State
--     execAction ws act = tryExec act $ ws ^. state


-- execAll2 :: State -> (forall act. (ActionStateExecutor act State) => [Maybe act] -> [WithStateMaybe act State])
-- execAll2 stInit actions = undefined where -- scanl execAction (WithState Nothing stInit) actions
--     -- execAction ws act = tryExec act $ ws ^. state


-- execAll2 :: State -> [forall act. (ActionStateExecutor act State) => Maybe act] -> State
-- execAll2 :: Behavior t State -> [forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)] -> Behavior t State
-- execAll2 stInit actions = undefined where -- scanl execAction (WithState Nothing stInit) actions
    -- execAction ws act = tryExec act $ ws ^. state


-- data ActionList t = forall act. (ActionStateExecutor act State) => ActionList [Behavior t (Maybe act)]

-- instance Monoid (ActionList t) where
--     mempty = ActionList []
--     -- (ActionList (a:as)) `mappend` (ActionList a') = ActionList (a : a')

-- appendAction :: (forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)) -> ActionList  t -> ActionList t
-- appendAction a (ActionList as) = ActionList (a:as)

-- pureAction :: (forall act. (ActionStateExecutor act State) => Behavior t (Maybe act)) -> ActionList t
-- pureAction a = ActionList [a]


-- appendAction :: forall act. ActionStateExecutor act State => act -> [act] -> [act]
-- appendAction = (:)





-- scanl :: (b -> a -> b) -> b -> [a] -> [b]


-- scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]


-- class SuperAction a where
--     trans :: st -> a -> st
--     upUI  :: a -> IO ()

-- appendSuperAction :: forall a. SuperAction a => a -> [a] -> [a]
-- appendSuperAction = (:)

-- instance SuperAction (WithStateMaybe AddRemove.Action State) where

-- appendSuperAction x xs = x : xs

-- -- act - typeclass

-- execList :: State -> [Maybe act]

-- execAll state list =

-- appendSuperAction nodeAddRemActionB
-- $ appendSuperAction nodeSelectionActionB
-- $ appendSuperAction nodeDragActionB
-- $ []
