module Empire.Commands.Graph.Code where

import Empire.Prelude hiding (range)

import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Empire.ASTOps.Parse           as ASTParse
import qualified Empire.ASTOps.Read            as ASTRead
import qualified Empire.Commands.Code          as Code
import qualified Empire.Commands.Publisher     as Publisher
import qualified Empire.Data.FileMetadata      as FileMetadata
import qualified Empire.Data.Graph             as Graph
import qualified Empire.Data.Library           as Library
import qualified Empire.Empire                 as Empire
import qualified LunaStudio.Data.GraphLocation as GraphLocation

import Control.Monad.Catch                  (handle)
import Data.Char                            (isSpace)
import Data.Text.Position                   (Delta)
import Empire.ASTOp                         (runASTOp)
import Empire.ASTOps.BreadcrumbHierarchy    (getMarker)
import Empire.Commands.Graph.Autolayout     (autolayout, autolayoutTopLevel)
import Empire.Commands.Graph.Breadcrumb     (makeGraph)
import Empire.Commands.Graph.Context        (withLibrary, withUnit)
import Empire.Commands.Graph.Metadata       (markFunctions, prepareNodeCache,
                                             readMetadata', removeMetadataNode,
                                             stripMetadata)
import Empire.Empire                        (Empire)
import Luna.Syntax.Text.Parser.State.Marker (TermMap (TermMap))
import LunaStudio.Data.Breadcrumb           (Breadcrumb (Breadcrumb),
                                             BreadcrumbItem (Definition))
import LunaStudio.Data.GraphLocation        (GraphLocation)
import LunaStudio.Data.NodeCache            (nodeIdMap, nodeMetaMap)
import LunaStudio.Data.Point                (Point)
import LunaStudio.Data.TextDiff             (TextDiff (TextDiff))


toDeltas :: Text -> [TextDiff] -> [(Delta, Delta, Text)]
toDeltas previousCode changes = map (toRealDelta . toDelta) changes where
    noMarkers = Code.removeMarkers previousCode
    toDelta (TextDiff range code _) = case range of
        Just (start, end) ->
            ( Code.pointToDelta start noMarkers
            , Code.pointToDelta end   noMarkers
            , code)
        _ -> (0, fromIntegral $ Text.length noMarkers, code)
    viewToReal c = if maybe False (not . isSpace . fst) $ Text.uncons c
            then Code.viewDeltasToReal
            else Code.viewDeltasToRealBeforeMarker
    toRealDelta (a,b,c) =
        let (a', b') = (viewToReal c) previousCode (a,b)
        in (a', b', c)

markText :: Text -> [TextDiff] -> Text
markText text diffs = Code.applyManyToText text changes where
    changes = toDeltas text diffs

substituteCode :: FilePath -> [TextDiff] -> Empire ()
substituteCode (GraphLocation.top -> gl) (breakDiffs -> diffs) = do
    code <- withUnit gl $ use Graph.code
    let noMarkers = Code.removeMarkers code
        t1 = (markText noMarkers diffs)
        t2 = (markText code diffs)
        newCode = Code.mergeMarkers t1 t2
    print t1
    print t2
    print newCode
    handle
        (\(e :: SomeException)
            -> withUnit gl $ Graph.userState . Graph.clsParseError ?= e)
        $ reloadCode gl newCode

loadCode :: GraphLocation -> Text -> Empire ()
loadCode gl code = do
    let topGl = GraphLocation.top gl
        file  = gl ^. GraphLocation.filePath
        libraryBody
            = Graph.userState . Empire.activeFiles . at file . traverse . Library.body
    (unit, grSt, scSt, exprMap) <- liftIO $ ASTParse.runProperParser code
    Graph.pmState . Graph.pmScheduler    .= scSt
    Graph.pmState . Graph.pmStage        .= grSt
    libraryBody   . Graph.clsClass       .= unit
    libraryBody   . Graph.clsCodeMarkers .= coerce exprMap
    libraryBody   . Graph.code           .= code
    libraryBody   . Graph.clsFuns        .= Map.empty

    (codeHadMeta, prevParseError) <- withUnit topGl $ do
        prevParseError <- use $ Graph.userState . Graph.clsParseError
        Graph.userState . Graph.clsParseError .= Nothing
        fileMetadata <- FileMetadata.toList <$> runASTOp readMetadata'
        let savedNodeMetas
                = Map.fromList $ map FileMetadata.toTuple fileMetadata
        Graph.userState . Graph.nodeCache . nodeMetaMap
            %= (\cache -> Map.union cache savedNodeMetas)
        runASTOp $ do
            let codeWithoutMeta = stripMetadata code
            Graph.code .= codeWithoutMeta
            metaRef <- ASTRead.getMetadataRef unit
            removeMetadataNode
            for_ metaRef deepDelete
            pure (codeWithoutMeta /= code, prevParseError)
    when (codeHadMeta && isJust prevParseError) $ resendCode topGl
    functions <- withUnit topGl $ do
        klass <- use $ Graph.userState . Graph.clsClass
        runASTOp $ do
            markFunctions klass
            funs <- ASTRead.classFunctions klass
            forM funs $ \f -> ASTRead.cutThroughDoc f >>= \fun ->
                matchExpr fun $ \case
                    Marked m _e -> do
                        marker <- getMarker =<< source m
                        uuid   <- use $ Graph.nodeCache . nodeIdMap . at marker
                        pure (uuid, f)
                    _ -> do
                        pure (Nothing, f)
    for_ functions $ \(lastUUID, fun) -> do
        uuid <- withLibrary file $ fst <$> makeGraph fun lastUUID
        let glToAutolayout = gl & GraphLocation.breadcrumb
                .~ Breadcrumb [Definition uuid]
        void $ autolayout glToAutolayout
    void $ autolayoutTopLevel topGl


reloadCode :: GraphLocation -> Text -> Empire ()
reloadCode gl code = do
    nodeCache <- prepareNodeCache gl
    withUnit (GraphLocation.top gl)
        $ Graph.userState . Graph.nodeCache .= nodeCache
    loadCode gl code

breakDiffs :: [TextDiff] -> [TextDiff]
breakDiffs diffs = go [] diffs where
    go acc [] = reverse acc
    go acc (d@(TextDiff range code cursor):list) =
        case Text.span isSpace code of
            (prefix, suffix)
                | Text.null prefix -> go (d:acc) list
                | otherwise        -> let
                        rangeEnd   = fromJust (fmap snd range)
                        newRange   = Just (rangeEnd, rangeEnd)
                        whitespace = TextDiff range prefix cursor
                        onlyCode   = TextDiff newRange suffix cursor
                    in go (onlyCode:whitespace:acc) list

resendCode :: GraphLocation -> Empire ()
resendCode gl = resendCodeWithCursor gl Nothing

resendCodeWithCursor :: GraphLocation -> Maybe Point -> Empire ()
resendCodeWithCursor gl cursor = getCode gl >>= \code ->
    Publisher.notifyCodeUpdate (gl ^. GraphLocation.filePath) code cursor

getCode :: GraphLocation -> Empire Text
getCode gl
    = Code.removeMarkers <$> withUnit (GraphLocation.top gl) (use Graph.code)
