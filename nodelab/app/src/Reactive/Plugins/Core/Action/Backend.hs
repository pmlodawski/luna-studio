module Reactive.Plugins.Core.Action.Backend where

import qualified Event.Backend as Backend
import           Event.Event
import           Object.Node
import           Utils.PreludePlus
import           BatchConnector.Connection
import           BatchConnector.Commands
import           BatchConnector.Updates
import           Batch.Project
import           Reactive.Plugins.Core.Action.Action

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
    execSt (MessageAction msg) = ActionUI $ ApplyUpdates [ handleMessage msg ]
    execSt OpenedAction        = ActionUI $ ApplyUpdates [ putStrLn "Connection Opened!"
                                                         , sendMessage listProjects
                                                         ]

handleMessage :: WebMessage -> IO ()
handleMessage (WebMessage topic bytes) = case topic of
    "project.list.status"   -> ensureProjectExists $ parseProjectsList bytes
    "project.create.update" -> print $ parseProjectCreateUpdate bytes
    _                       -> return ()

ensureProjectExists :: Maybe [Project] -> IO ()
ensureProjectExists Nothing         = putStrLn "Message parse error!"
ensureProjectExists (Just projects) = if   null projects
                                      then createFirstProject >> putStrLn "Creating project!"
                                      else putStrLn "Projects exist!" >> print projects

createFirstProject :: IO ()
createFirstProject  = sendMessage $ createProject "myFirstProject" "some/path"

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates updates) state) = sequence_ updates
