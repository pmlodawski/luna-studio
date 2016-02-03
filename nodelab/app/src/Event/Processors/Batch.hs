module Event.Processors.Batch where

import           Utils.PreludePlus
import qualified Event.Event                as Event
import           Event.Connection           as Connection
import           Event.Batch                as Batch
import           BatchConnector.Connection  (WebMessage(..), ControlCode(..))
import           Data.Binary (decode)
import           Empire.API.Topic as Topic

process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes)
    | topic == Topic.addNodeUpdate        = NodeAdded         $ decode bytes
    | topic == Topic.removeNodeUpdate     = NodeRemoved       $ decode bytes
    | topic == Topic.connectUpdate        = NodesConnected    $ decode bytes
    | topic == Topic.disconnectUpdate     = NodesDisconnected $ decode bytes
    | topic == Topic.updateNodeMetaUpdate = NodeMetaUpdated   $ decode bytes
    | topic == Topic.nodeUpdate           = NodeUpdated       $ decode bytes
    | topic == Topic.programStatus        = ProgramFetched    $ decode bytes
    | topic == Topic.codeUpdate           = CodeUpdated       $ decode bytes
    | topic == Topic.nodeResultUpdate     = NodeResultUpdated $ decode bytes
    | topic == Topic.listProjectsStatus   = ProjectList       $ decode bytes
    | topic == Topic.createProjectUpdate  = ProjectCreated    $ decode bytes
    | topic == Topic.renameNodeUpdate     = NodeRenamed       $ decode bytes
    | otherwise                           = UnknownEvent topic
    -- "project.open.update"                                          -> ProjectOpened <$> parseProjectOpenUpdate bytes
    -- "project.open.error"                                           -> Just $ ProjectDoesNotExist
    -- "project.library.list.status"                                  -> LibrariesList <$> parseLibrariesListResponse bytes
    -- "project.library.create.update"                                -> LibraryCreated <$> parseLibraryCreateResponse bytes
    -- "project.library.ast.function.add.update"                      -> WorkspaceCreated <$> parseFunctionCreateResponse bytes
    -- "interpreter.value.update"                                     -> uncurry ValueUpdate <$> parseValueUpdate bytes
    -- "project.library.ast.function.graph.node.add.update"           -> NodeAdded <$> parseAddNodeResponse bytes
    -- "project.library.ast.function.graph.node.remove.update"        -> Just NodeRemoved
    -- "project.library.ast.function.graph.node.modifyinplace.update" -> Just NodeModified
    -- "project.library.ast.function.graph.node.add.fakeres"          -> NodeAdded <$> parseAddNodeFakeResponse bytes
    -- "project.library.ast.function.graph.node.default.set.update"   -> Just NodeDefaultUpdated
    -- "project.library.ast.code.get.status"                          -> CodeUpdate <$> parseGetCodeResponse bytes
    -- "project.library.ast.code.set.update"                          -> Just CodeSet
    -- "project.library.ast.code.set.error"                           -> Just $ CodeSetError $ parseErrorMsg bytes
    -- "project.library.ast.function.graph.connect.update"            -> Just NodesConnected
    -- "project.library.ast.function.graph.disconnect.update"         -> Just NodesDisconnected
    -- "project.library.ast.get.status"                               -> Just ASTElementExists
    -- "project.library.ast.get.error"                                -> Just ASTElementDoesNotExist
    -- "interpreter.run.update"                                       -> RunFinished <$> parseRunStatus bytes
    -- "project.library.ast.function.graph.get.status"                -> uncurry GraphViewFetched <$> parseGraphViewResponse bytes
    -- "interpreter.getprojectid.status"                              -> InterpreterGotProjectId <$> parseProjectIdStatus bytes
    -- "interpreter.serializationmode.insert.update"                  -> Just SerializationModeInserted
    -- _                                                              -> UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
