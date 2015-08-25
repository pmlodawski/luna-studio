module Reactive.Plugins.Core.Action.Backend where

import           Utils.PreludePlus
import           Reactive.Plugins.Core.Action.Action
import           Event.Event
import           Object.Node
import           BatchConnector.Connection
import           BatchConnector.Commands
import qualified Event.Backend as Backend
import           BatchConnector.Updates

data Action = MessageAction { _msg   :: WebMessage }
            | OpenedAction
            | ApplyUpdates  { _actions :: [IO ()] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "BackendAction"

toAction :: Event Node -> Maybe Action
toAction (Backend event) = Just $ case event of
    Backend.Message msg -> MessageAction msg
    Backend.Opened      -> OpenedAction
toAction _                 = Nothing

instance ActionStateUpdater Action where
    execSt (MessageAction msg) = ActionUI $ ApplyUpdates [ print msg
                                                         , handleMessage msg
                                                         ]
    execSt OpenedAction        = ActionUI $ ApplyUpdates [ putStrLn "Connection Opened!"
                                                         , sendMessage listProjects
                                                         ]

handleMessage :: WebMessage -> IO ()
handleMessage (WebMessage topic bytes) = case topic of
    "project.list.status" -> print $ parseProjectsList bytes
    _                     -> return ()

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates updates) state) = sequence_ updates
