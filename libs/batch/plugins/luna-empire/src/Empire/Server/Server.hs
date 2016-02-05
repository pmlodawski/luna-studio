module Empire.Server.Server where

import           Control.Monad.State           (StateT)
import           Data.Binary                   (Binary)
import qualified Data.Binary                   as Bin
import           Data.ByteString.Lazy          (toStrict)
import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library       (LibraryId)
import           Empire.API.Data.Project       (ProjectId)
import           Empire.Env                    (Env)
import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT (..))
import qualified Flowbox.Bus.Data.Flag         as Flag
import qualified Flowbox.Bus.Data.Message      as Message
import           Prologue

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = void . lift $ BusT $ Bus.send Flag.Enable $ Message.Message topic $ toStrict $ Bin.encode bin

withGraphLocation :: (ProjectId -> LibraryId -> a) -> GraphLocation -> a
withGraphLocation f graphLocation = f (graphLocation ^. GraphLocation.projectId)
                                      (graphLocation ^. GraphLocation.libraryId)

errorMessage :: String
errorMessage = "Error processing request: "