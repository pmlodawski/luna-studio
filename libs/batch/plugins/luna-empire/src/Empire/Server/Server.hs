module Empire.Server.Server where

import           Prologue
import           Empire.API.Data.GraphLocation   (GraphLocation)
import qualified Empire.API.Data.GraphLocation   as GraphLocation
import           Empire.API.Data.Library         (LibraryId)
import           Empire.API.Data.Project         (ProjectId)

withGraphLocation :: (ProjectId -> LibraryId -> a) -> GraphLocation -> a
withGraphLocation f graphLocation = f (graphLocation ^. GraphLocation.projectId)
                                      (graphLocation ^. GraphLocation.libraryId)

errorMessage :: String
errorMessage = "Error processing request: "
