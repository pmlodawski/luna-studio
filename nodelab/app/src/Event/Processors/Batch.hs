module Event.Processors.Batch where

import           Utils.PreludePlus
import qualified Event.Event               as Event
import           Event.Connection          as Connection
import           Event.Batch               as Batch
import           Data.Dynamic
import           BatchConnector.Connection (WebMessage(..), ControlCode(..))
import           BatchConnector.Updates

process :: Event.Event Dynamic -> Maybe (Event.Event Dynamic)
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = rescueParseError topic $ case topic of
    "project.list.status"                                 -> ProjectsList <$> parseProjectsListResponse bytes
    "project.create.update"                               -> ProjectCreated <$> parseProjectCreateUpdate bytes
    "project.open.update"                                 -> ProjectOpened <$> parseProjectOpenUpdate bytes
    "project.open.error"                                  -> Just $ ProjectDoesNotExist
    "project.library.list.status"                         -> LibrariesList <$> parseLibrariesListResponse bytes
    "project.library.create.update"                       -> LibraryCreated <$> parseLibraryCreateResponse bytes
    "project.library.ast.function.add.update"             -> WorkspaceCreated <$> parseFunctionCreateResponse bytes
    "interpreter.value.update"                            -> uncurry ValueUpdate <$> parseValueUpdate bytes
    "project.library.ast.function.graph.node.add.update"  -> NodeAdded <$> parseAddNodeResponse bytes
    "project.library.ast.function.graph.node.add.fakeres" -> NodeAdded <$> parseAddNodeFakeResponse bytes
    "project.library.ast.code.get.status"                 -> CodeUpdate <$> parseGetCodeResponse bytes
    "project.library.ast.function.graph.connect.update"   -> Just $ NodesConnected
    "project.library.ast.get.status"                      -> Just ASTElementExists
    "project.library.ast.get.error"                       -> Just ASTElementDoesNotExist
    "interpreter.run.update"                              -> Just RunFinished
    "project.library.ast.function.graph.get.status"       -> uncurry GraphViewFetched <$> parseGraphViewResponse bytes
    "interpreter.getprojectid.status"                     -> InterpreterGotProjectId <$> parseProjectIdStatus bytes
    "interpreter.serializationmode.insert.update"         -> Just $ SerializationModeInserted
    _                                                     -> Just $ UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened

rescueParseError :: String -> Maybe Batch.Event -> Batch.Event
rescueParseError _     (Just ev) = ev
rescueParseError topic Nothing   = ParseError topic
