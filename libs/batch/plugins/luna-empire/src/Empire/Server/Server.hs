module Empire.Server.Server where

import           Prologue
import           Data.Binary                     (Binary)
import qualified Data.Binary                     as Bin
import           Data.ByteString                 (ByteString)
import           Data.ByteString.Lazy            (fromStrict, toStrict)
import           Control.Monad.State             (StateT)
import           Flowbox.Bus.BusT                (BusT (..))
import qualified Flowbox.Bus.Bus                 as Bus
import qualified Flowbox.Bus.Data.Flag           as Flag
import qualified Flowbox.Bus.Data.Message        as Message
import           Empire.Env                      (Env)
import           Empire.API.Data.GraphLocation   (GraphLocation)
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Library         (LibraryId)
import           Empire.API.Data.Project         (ProjectId)

sendToBus :: Binary a => String -> a -> StateT Env BusT ()
sendToBus topic bin = void . lift $ BusT $ Bus.send Flag.Enable $ Message.Message topic $ toStrict $ Bin.encode bin

withGraphLocation :: (ProjectId -> LibraryId -> a) -> GraphLocation -> a
withGraphLocation f graphLocation = f (graphLocation ^. GraphLocation.projectId)
                                      (graphLocation ^. GraphLocation.libraryId)

errorMessage :: String
errorMessage = "Error processing request: "
