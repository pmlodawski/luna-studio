module Empire.Commands.Publisher where

import Prologue
import Empire.Empire
import Control.Monad.Reader
import Control.Monad.STM             (atomically)
import Control.Concurrent.STM.TChan  (writeTChan)
import Empire.API.Data.AsyncUpdate   (AsyncUpdate (..))
import Empire.API.Data.GraphLocation (GraphLocation)
import Empire.API.Data.Node          (Node)

notifyNodeUpdate :: GraphLocation -> Node -> Command s ()
notifyNodeUpdate loc n = do
    chan <- asks $ view updatesChan
    liftIO $ atomically $ writeTChan chan $ NodeUpdate loc n
