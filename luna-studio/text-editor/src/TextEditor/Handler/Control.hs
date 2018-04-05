module TextEditor.Handler.Control
    ( handle
    ) where

import           Common.Action.Command              (Command)
import           Common.Prelude
import           Control.Concurrent                 (threadDelay)
import           JS.Atom                            (activeLocation, openedFiles, pushInterpreterUpdate)
import qualified LunaStudio.API.Control.Interpreter as Interpreter
import qualified LunaStudio.API.Response            as Response
import qualified LunaStudio.Data.GraphLocation      as GraphLocation
import           TextEditor.Action.Batch            (interpreterPause, interpreterReload, interpreterStart, openFile)
import           TextEditor.Event.Batch             (BatchEvent (EmpireStarted, InterpreterResponse, InterpreterUpdate))
import           TextEditor.Event.Event             (Event (Atom, Batch))
import           TextEditor.Event.Internal          (InternalEvent (InterpreterPause, InterpreterReload, InterpreterStart))
import           TextEditor.Handler.Backend.Common  (doNothing2, handleResponse)
import           TextEditor.State.Global            (State)


handle :: Event -> Maybe (Command State ())
handle (Batch (InterpreterUpdate (Interpreter.Update update))) = Just $ pushInterpreterUpdate "Update" $ Just update
handle (Batch (InterpreterResponse response)) = Just $ handleResponse response success doNothing2 where
    success _ = pushInterpreterUpdate (head $ words $ show $ response ^. Response.request) Nothing
handle (Batch (EmpireStarted _)) = Just $ do
    openedFile <- activeLocation
    files <- openedFiles
    liftIO $ print files
    liftIO $ threadDelay 5000000
    liftIO $ putStrLn "textedit"
    withJust openedFile $ \gl -> do
        liftIO $ putStrLn "textedit" >> print gl
        openFile $ gl ^. GraphLocation.filePath
handle (Atom InterpreterStart ) = Just $ interpreterStart
handle (Atom InterpreterPause ) = Just $ interpreterPause
handle (Atom InterpreterReload) = Just $ interpreterReload
handle _ = Nothing
