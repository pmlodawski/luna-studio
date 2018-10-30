module Empire.Commands.Graph.CopyPaste where

import Empire.Prelude

import qualified Data.Map                        as Map
import qualified Data.Set                        as Set
import qualified Data.Text                       as Text
import qualified Empire.ASTOps.Read              as ASTRead
import qualified Empire.Commands.AST             as AST
import qualified Empire.Commands.Code            as Code
import           Empire.Commands.Graph.Context   (typecheck, withGraph, withUnit)
import           Empire.Commands.Graph.Code      (reloadCode, resendCode)
import qualified Empire.Commands.Graph.Metadata  as Metadata
import           Data.Text.Span                  (SpacedSpan (..), leftSpacedSpan)
import qualified Empire.Data.BreadcrumbHierarchy as BH
import qualified LunaStudio.Data.Breadcrumb      as Breadcrumb
import qualified Empire.Data.Graph               as Graph
import qualified LunaStudio.Data.GraphLocation        as GraphLocation
import LunaStudio.Data.GraphLocation        (GraphLocation (..), (|>))
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan as CodeSpan
import Empire.Empire
import LunaStudio.Data.Node                 (ExpressionNode (..), NodeId)
import LunaStudio.Data.Breadcrumb           (Breadcrumb (..), BreadcrumbItem,
                                             Named)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import Empire.ASTOp                         (ClassOp, GraphOp,
                                             liftScheduler, runASTOp,
                                             runAliasAnalysis)
import qualified Safe
import Empire.Data.FileMetadata             (FileMetadata (FileMetadata),
                                             MarkerNodeMeta (MarkerNodeMeta))
import qualified Empire.Data.FileMetadata as FM
import qualified LunaStudio.Data.Position             as Position
import qualified LunaStudio.Data.NodeMeta             as NodeMeta
import qualified LunaStudio.Data.NodeCache            as NodeCache
import qualified Data.Text.Span                       as Span
import Empire.Data.Layers                   (SpanLength, SpanOffset)
import LunaStudio.Data.Position             (Position)
import Empire.Data.AST                      (ConnectionException (..), EdgeRef,
                                             InvalidConnectionException (..),
                                             NodeRef,
                                             NotInputEdgeException (..),
                                             NotUnifyException,
                                             PortDoesNotExistException (..),
                                             SomeASTException,
                                             astExceptionFromException,
                                             astExceptionToException)
import LunaStudio.Data.NodeMeta             (NodeMeta)


copyNodes :: GraphLocation -> [NodeId] -> Empire Text
copyNodes loc@(GraphLocation _ (Breadcrumb [])) nodeIds = withUnit loc $ do
    clipboard <- runASTOp $ do
        starts  <- mapM Code.functionBlockStart nodeIds
        (lengths, metadata) <- do
            funs  <- use Graph.clsFuns
            let names       = map (\a -> (a, Map.lookup a funs)) nodeIds
                nonExistent = filter (isNothing . snd) names
            forM nonExistent $ \(nid, _) ->
                throwM $ BH.BreadcrumbDoesNotExistException (Breadcrumb [Breadcrumb.Definition nid])
            refs <- mapM ASTRead.getFunByNodeId [ nid | (nid, Just (view Graph.funName -> _name)) <- names]
            unzip <$> forM refs (\ref -> do
                LeftSpacedSpan (SpacedSpan _off len) <- view CodeSpan.realSpan <$> getLayer @CodeSpan ref
                metadata <- Metadata.extractMarkedMetasAndIds ref
                return $ (fromIntegral len, metadata))
        codes <- mapM
            (\(start, len) -> Code.getAt start (start + len)) $
            zip starts lengths
        let metaInfo          = FileMetadata [ MarkerNodeMeta marker meta
                | (marker, (Just meta, _)) <- concat metadata ]
            metaSection       = Metadata.renderMetadata metaInfo
        return $ Text.concat [Text.unlines codes, metaSection]
    return clipboard
copyNodes loc nodeIds = withGraph loc $ do
    indent      <- runASTOp Code.getCurrentIndentationLength
    codeAndMeta <- runASTOp $ forM nodeIds $ \nid -> do
        ref      <- ASTRead.getASTRef nid
        code     <- Code.getCodeWithIndentOf ref
        metadata <- Metadata.extractMarkedMetasAndIds ref
        return (code, metadata)
    let (codes, metadata) = unzip codeAndMeta
        metaInfo          = FileMetadata [ MarkerNodeMeta marker meta
            | (marker, (Just meta, _)) <- concat metadata ]
        metaSection       = Metadata.renderMetadata metaInfo
    return $ Text.concat [Text.unlines (map (unindent (fromIntegral indent)) codes), metaSection]

unindent :: Int -> Text -> Text
unindent offset code = Text.intercalate "\n" $ map (Text.drop offset) $ Text.lines code


indent :: Int -> Text -> Text
indent offset (Text.lines -> code) =
    Text.intercalate "\n" $ map (\line -> Text.concat [Text.replicate offset " ", line]) code


pasteNodes :: GraphLocation -> Position -> String -> Empire ()
pasteNodes loc@(GraphLocation file (Breadcrumb [])) position (Text.pack -> code) = do
    newCode <- withUnit loc $ runASTOp $ do
        unit  <- use Graph.clsClass
        funs  <- ASTRead.classFunctions unit
        funStarts <- forM funs $ \fun -> do
            xFun <- view (NodeMeta.position . Position.x) <$> (fromMaybe def <$> AST.readMeta fun)
            if xFun < position ^. Position.x then return Nothing else do
                funStart <- Code.functionBlockStartRef fun
                return $ Just funStart
        let cleanCode = Code.removeMarkers code
        let spacedCode = cleanCode <> "\n"
        case Safe.headMay (catMaybes funStarts) of
            Just codePosition -> Code.applyDiff codePosition codePosition spacedCode
            _                 -> do
                unitSpan <- getLayer @CodeSpan unit
                let endOfFile = view (CodeSpan.realSpan . Span.length) unitSpan
                Code.applyDiff endOfFile endOfFile $ Text.cons '\n' cleanCode
    reloadCode loc newCode
    resendCode loc
pasteNodes loc position (Text.pack -> clipboard') = do
    (newCode, remarkeredMeta) <- withGraph loc $ runASTOp $ do
        indentation <- fromIntegral <$> Code.getCurrentIndentationLength
        oldSeq             <- ASTRead.getCurrentBody
        nodes              <- AST.readSeq oldSeq
        nodesAndMetas      <- mapM (\n -> (n,) <$> AST.readMeta n) nodes
        let nodesWithMetas =  mapMaybe (\(n,m) -> (n,) <$> m) nodesAndMetas
        nearestNode        <- findPreviousNodeInSequence oldSeq
            (def & NodeMeta.position .~ position) nodesWithMetas
        let (clipboard, meta) = Metadata.stripMetadata' clipboard'
        metaInfo <- view (to FM.metas) <$> Metadata.parseMetadata meta
        let absoluteMetaInfo = moveToOrigin metaInfo
        existingMarkers    <- Code.extractMarkers <$> use Graph.code
        let (remarkered, newMarkers) =
                if Set.null (Code.extractMarkers clipboard)
                then (clipboard, def)
                else Code.remarkerCode clipboard existingMarkers
            fixupMetaInfo (MarkerNodeMeta m meta) =
                (fromMaybe m (Map.lookup m newMarkers),
                 meta & NodeMeta.position %~ Position.move (coerce position))
            remarkeredMeta = Map.fromList $ map fixupMetaInfo absoluteMetaInfo
        newCode <- case nearestNode of
            Just ref -> do
                Just beg <- Code.getAnyBeginningOf ref
                len <- getLayer @SpanLength ref
                let code = Text.cons '\n' $ indent indentation remarkered
                Code.applyDiff (beg+len) (beg+len) code
            _        -> do
                beg <- Code.getCurrentBlockBeginning
                let code = Text.concat [
                          Text.stripStart (indent indentation remarkered)
                        , "\n"
                        , indent indentation "\n"
                        ]
                Code.applyDiff beg beg code
        pure (newCode, remarkeredMeta)
    withUnit (GraphLocation.top loc) $ do
        Graph.userState . Graph.nodeCache . NodeCache.nodeMetaMap %= \prev ->
            Map.union prev remarkeredMeta
    reloadCode loc newCode
    typecheck loc
    resendCode loc


moveToOrigin :: [MarkerNodeMeta] -> [MarkerNodeMeta]
moveToOrigin metas' = map
    (\(MarkerNodeMeta m me) ->
        MarkerNodeMeta m (me & NodeMeta.position %~
            Position.move
                (coerce (Position.rescale leftTopCorner (-1))))) metas'
    where
        leftTopCorner = fromMaybe (Position.fromTuple (0,0))
                      $ Position.leftTopPoint
                      $ map (\mnm -> FM.meta mnm ^. NodeMeta.position) metas'


findPreviousNodeInSequence :: NodeRef -> NodeMeta -> [(NodeRef, NodeMeta)] -> GraphOp (Maybe NodeRef)
findPreviousNodeInSequence seq meta nodes = do
    let position           = Position.toTuple $ view NodeMeta.position meta
        nodesWithPositions = map (\(n, m) -> (n, Position.toTuple $ m ^. NodeMeta.position)) nodes
        nodesToTheLeft     = filter (\(n, (x, y)) -> x <= fst position) nodesWithPositions
    findPreviousSeq seq (Set.fromList $ map fst nodesToTheLeft)


findPreviousSeq :: NodeRef -> Set.Set NodeRef -> GraphOp (Maybe NodeRef)
findPreviousSeq seq nodesToTheLeft = matchExpr seq $ \case
    Seq l r -> do
        l' <- source l
        r' <- source r
        if Set.member r' nodesToTheLeft then return (Just r') else findPreviousSeq l' nodesToTheLeft
    _ -> return $ if Set.member seq nodesToTheLeft then Just seq else Nothing
