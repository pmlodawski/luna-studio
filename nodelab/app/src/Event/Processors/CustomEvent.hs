module Event.Processors.CustomEvent (process) where

import           Data.Binary                (Binary, decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Map.Lazy              as Map
import           Data.Monoid                (Last (..))
import           Utils.PreludePlus

import           BatchConnector.Connection  (ControlCode (..), WebMessage (..))
import           Empire.API.Response        as Response
import           Empire.API.Topic           as Topic
import           Event.Batch                as Batch
import           Event.Connection           as Connection
import           Event.CustomEvent          as CustomEvent
import           Event.Debug                (Event (..))
import qualified Event.Event                as Event


process :: Event.Event -> IO (Maybe Event.Event)
process (Event.CustomEvent (CustomEvent.RawEvent "debug.getState" _)) = return $ Just $ Event.Debug $ GetState
process _                                                             = return Nothing
