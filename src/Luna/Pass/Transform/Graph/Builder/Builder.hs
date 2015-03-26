---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Luna.Pass.Transform.Graph.Builder.Builder where

import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.Maybe                 as Maybe

import           Flowbox.Prelude                          hiding (Traversal, error, mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.Data.StructInfo                     (StructInfo)
import           Luna.Pass.Transform.Graph.Builder.ArgRef (ArgRef)
import qualified Luna.Pass.Transform.Graph.Builder.ArgRef as ArgRef
import           Luna.Pass.Transform.Graph.Builder.State  (GBPass)
import qualified Luna.Pass.Transform.Graph.Builder.State  as State
import           Luna.Syntax.Arg                          (Arg (Arg))
import qualified Luna.Syntax.Decl                         as Decl
import           Luna.Syntax.Enum                         (Enumerated)
import qualified Luna.Syntax.Enum                         as Enum
import           Luna.Syntax.Expr                         (LExpr)
import qualified Luna.Syntax.Expr                         as Expr
import qualified Luna.Syntax.Graph.DefaultsMap            as DefaultsMap
import           Luna.Syntax.Graph.Graph                  (Graph)
import qualified Luna.Syntax.Graph.Node                   as Node
import           Luna.Syntax.Graph.Node.Expr              (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr              as NodeExpr
import qualified Luna.Syntax.Graph.Node.MultiPart         as MultiPart
import qualified Luna.Syntax.Graph.Node.StringExpr        as StringExpr
import           Luna.Syntax.Graph.Port                   (DstPort, SrcPort)
import qualified Luna.Syntax.Graph.Port                   as Port
import           Luna.Syntax.Graph.Tag                    (TDecl, TExpr, TPat, Tag)
import           Luna.Syntax.Label                        (Label (Label))
import qualified Luna.Syntax.Label                        as Label
import           Luna.Syntax.Lit                          (LLit)
import qualified Luna.Syntax.Name.Pattern                 as Pattern
import           Luna.Util.LunaShow                       (LunaShow, lunaShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


type LunaExpr ae = (Enumerated ae, LunaShow (LExpr ae V), LunaShow (LLit ae))
type V = ()

inputsID, outputID :: Node.ID
inputsID = -2
outputID = -1


run :: Monad m => StructInfo -> TDecl V -> EitherT State.Error m (TDecl V, Graph Tag V)
run aliasInfo lexpr = evalStateT (func2graph lexpr) $ State.mk aliasInfo inputsID


func2graph :: TDecl V -> GBPass V m (TDecl V, Graph Tag V)
func2graph decl@(Label _ (Decl.Func (Decl.FuncDecl _ sig _ body))) = do
    State.initFreeNodeID decl
    State.insNode (inputsID, Node.mkInputs)
    State.insNode (outputID, Node.mkOutputs)
    sig'  <- buildInputsArgs sig
    body' <- buildBody body
    decl' <- State.saveFreeNodeID decl
    (,) <$> return (decl' & Label.element . Decl.funcDecl . Decl.funcDeclSig  .~ sig'
                          & Label.element . Decl.funcDecl . Decl.funcDeclBody .~ body')
        <*> State.getGraph


buildInputsArgs :: Pattern.ArgPat Tag e -> GBPass V m (Pattern.ArgPat Tag e)
buildInputsArgs (Pattern.NamePat prefix base segmentList) = flip evalStateT (0::Int) $ do
    prefix' <- case prefix of
        Nothing -> return Nothing
        Just pr -> Just <$> buildInputsArg pr
    base'        <- parseInputsSegment base
    segmentList' <- mapM parseInputsSegment segmentList
    return $ Pattern.NamePat prefix' base' segmentList'
    where
        parseInputsSegment (Pattern.Segment sBase sArgs) =
            Pattern.Segment sBase <$> mapM buildInputsArg sArgs

        buildInputsArg arg@(Arg pat _) = do
            no <- get
            put $ no + 1
            lift $ State.addToNodeMap (Enum.id $ pat ^. Label.label) (inputsID, Port.mkSrc no)
            return arg


buildBody :: [TExpr V] -> GBPass V m [TExpr V]
buildBody []   = State.connectMonadic outputID >> return []
buildBody body = do
    body' <- mapM (fmap (view _1) . flip buildNode Nothing) (init body)
    let output = last body
    output' <- buildOutput output
    return $ body' ++ [output']


buildOutput :: TExpr V -> GBPass V m (TExpr V)
buildOutput lexpr = do
    lexpr' <- case unwrap lexpr of
        Expr.Assignment {}                        -> view _1 <$> buildNode lexpr Nothing
        --Expr.Tuple   items                        -> buildAndConnectMany True  True Nothing outputID items 0
        --Expr.Grouped (Label _ (Expr.Tuple items)) -> buildAndConnectMany True  True Nothing outputID items 0
        --Expr.Grouped v@(Label _ (Expr.Var {}))    -> buildAndConnect     True  True Nothing outputID (v, Port.Num 0)
        --Expr.Grouped V                            -> buildAndConnect     False True Nothing outputID (v, Port.Num 0)
        --Expr.Var {}                               -> buildAndConnect     True  True Nothing outputID (lexpr, Port.All)
        --_                                         -> buildAndConnect     False True Nothing outputID (lexpr, Port.All)
        _   ->  do (lexpr', nodeID, srcPort) <- buildNode lexpr Nothing
                   State.connect nodeID srcPort outputID Port.mkDstAll
                   return lexpr'
    State.connectMonadic outputID
    return lexpr'


buildExprApp :: Expr.ExprApp Tag V -> GBPass V m (Expr.ExprApp Tag V, [ArgRef])
buildExprApp (Pattern.NamePat prefix base segmentList) = flip runStateT [] $ do
    prefix' <- case prefix of
        Nothing     -> return Nothing
        Just appArg -> Just <$> buildAppArg appArg
    base' <- buildBase base
    segmentList' <- mapM buildSegment segmentList
    modify reverse
    return $ Pattern.NamePat prefix' base' segmentList'
    where
        addArg arg = modify (arg:)

        buildBase (Pattern.Segment sBase sArgs) = do
            --port <- Port.mkDst . length <$> get
            --(sBase', arg) <- lift $ processArg (sBase, port)
            --addArg arg
            sArgs' <- mapM buildAppArg sArgs
            return $ Pattern.Segment sBase sArgs'

        buildSegment (Pattern.Segment sBase sArgs) =
            Pattern.Segment sBase <$> mapM buildAppArg sArgs

        buildAppArg (Expr.AppArg argName e) = do
            port <- Port.mkDst . length <$> get
            (e', arg) <- lift $ processArg (e, port)
            addArg arg
            return $ Expr.AppArg argName e'


buildNode :: TExpr V -> Maybe TPat -> GBPass V m (TExpr V, Node.ID, SrcPort)
buildNode lexpr outputName = case unwrap lexpr of
    Expr.Assignment dst src -> do
        (src', nodeID, srcPort) <- buildNode src (Just dst)
        State.registerIDs lexpr (nodeID, Port.mkSrcAll)
        return (Label tag $ Expr.Assignment dst src', nodeID, srcPort)
    Expr.Tuple items -> do
        (items', argRefs) <- processArgs items
        let lexpr' = Label tag $ Expr.Tuple items'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.Tuple) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.List (Expr.SeqList items) -> do
        (items', argRefs) <- processArgs items
        let lexpr' = Label tag $ Expr.List $ Expr.SeqList items'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.List) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.App exprApp -> do
        (exprApp', argRefs) <- buildExprApp exprApp
        let mp     = MultiPart.fromNamePat exprApp'
            lexpr' = Label tag $ Expr.App exprApp'
        (le, ni)  <- addNodeWithExpr lexpr' outputName (NodeExpr.MultiPart mp) argRefs
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    Expr.Var (Expr.Variable vname _) -> State.gvmNodeMapLookUp (Enum.id tag) >>= \case
        Nothing             -> do (le, ni) <- addNode lexpr outputName []
                                  return (le, ni, Port.mkSrcAll)
        Just (srcNID, srcPort) -> if Maybe.isJust outputName
            then do let nodeExpr = NodeExpr.StringExpr $ StringExpr.fromString "_pattern_match_"
                    (le, ni) <- addNodeWithExpr lexpr outputName nodeExpr []
                    State.connect srcNID srcPort ni Port.mkDstAll
                    return (le, ni, Port.mkSrcAll)
            else do State.registerIDs lexpr (srcNID, srcPort)
                    return (lexpr, srcNID, srcPort)
    _ -> do
        (le, ni) <- addNode lexpr outputName []
        State.registerIDs le (ni, Port.mkSrcAll)
        return (le, ni, Port.mkSrcAll)
    where
        tag = lexpr ^. Label.label

processArgs :: [TExpr V] -> GBPass V m ([TExpr V], [ArgRef])
processArgs items = do
    itemsArgs <- mapM processArg $ zip items $ map Port.mkDst [0..]
    let items'  = map fst itemsArgs
        argRefs = map snd itemsArgs
    return (items', argRefs)


processArg :: (TExpr V, DstPort) -> GBPass V m (TExpr V, ArgRef)
processArg (lexpr, dstPort) = if constainsVar lexpr
    then do (lexpr', nodeID, srcPort) <- buildNode lexpr Nothing
            return (lexpr', ArgRef.mkNode (nodeID, srcPort, dstPort))
    else return (lexpr, ArgRef.mkDefault (dstPort, lexpr))


addNode :: TExpr V -> Maybe TPat -> [ArgRef] -> GBPass V m (TExpr V, Node.ID)
addNode lexpr outputName argRefs = addNodeWithExpr lexpr outputName nodeExpr argRefs
    where nodeExpr = NodeExpr.StringExpr $ StringExpr.fromString $ lunaShow lexpr


addNodeWithExpr :: TExpr V -> Maybe TPat
                -> NodeExpr Tag V -> [ArgRef] -> GBPass V m (TExpr V, Node.ID)
addNodeWithExpr lexpr outputName nodeExpr argRefs = do
    (nodeID, position, lexpr') <- State.getNodeInfo lexpr
    let defaults = ArgRef.defaults argRefs
        nodes    = ArgRef.nodes    argRefs
        node = Node.Expr nodeExpr outputName (DefaultsMap.fromList defaults) position
    State.insNode (nodeID, node)
    State.connectMonadic nodeID
    mapM_ (\(srcID, srcPort, dstPort) -> State.connect srcID srcPort nodeID dstPort) nodes
    return (lexpr', nodeID)


constainsVar _ = True




















































--buildNode :: LunaExpr a v
--          => Bool -> Bool -> Maybe VNameP -> LExpr a V -> GBPass a V m AST.ID
--buildNode astFolded monadicBind outName lexpr = case unwrap lexpr of
--    Expr.Assignment pat dst              -> buildAssignment pat dst
--    Expr.App        exprApp              -> buildApp exprApp
--    --Expr.Accessor   acc dst              -> addExprNode (view Expr.accName acc) [dst]
--    Expr.Var        name                 -> buildVar name
--    --Expr.NativeVar  name                 -> buildVar name
--    Expr.Cons       name                 -> addExprNode (toString name) []
--    Expr.Lit        lvalue               -> addExprNode (lunaShow lvalue) []
--    Expr.Tuple      items                -> addNodeHandleFlags (NodeExpr.StringExpr StringExpr.Tuple) items
--    Expr.List       (Expr.RangeList {})  -> showAndAddNode
--    Expr.List       (Expr.SeqList items) -> addNodeHandleFlags (NodeExpr.StringExpr StringExpr.List) items
--    --Expr.Native     segments             -> addNodeHandleFlags (NodeExpr.StringExpr $ StringExpr.Native $ showNative lexpr) $ filter isNativeVar segments
--    Expr.Wildcard                        -> lift $ left $ "GraphBuilder.buildNode: Unexpected Expr.Wildcard with id=" ++ show nodeID
--    Expr.Grouped    grouped              -> buildGrouped grouped
--    _                                    -> showAndAddNode
--    where
--        nodeID = getID lexpr where
--            getID (Label l (Expr.App exprApp)) = getID $ exprApp ^. Pattern.base . Pattern.segmentBase
--            getID (Label l _                 ) = Enum.id l

--        --buildAssignment pat dst = do
--        --    let patStr = lunaShow pat
--        --    realPat <- isRealPat pat dst
--        --    if realPat
--        --        then do patIDs <- buildPat pat
--        --                let nodeExpr = NodeExpr.StringExpr $ StringExpr.Pattern patStr
--        --                    node     = Node.Expr nodeExpr (genName nodeExpr nodeID)
--        --                State.insNodeWithFlags (nodeID, node) astFolded assignment
--        --                case patIDs of
--        --                   [patID] -> State.addToNodeMap patID (nodeID, Port.All)
--        --                   _       -> mapM_ (\(n, patID) -> State.addToNodeMap patID (nodeID, Port.Num n)) $ zip [0..] patIDs
--        --                dstID <- buildNode True True Nothing dst
--        --                State.connect dstID nodeID $ Port.Num 0
--        --                connectMonadic nodeID
--        --                return nodeID
--        --        else do [p] <- buildPat pat
--        --                j <- buildNode False True (Just patStr) dst
--        --                State.addToNodeMap p (j, Port.All)
--        --                return j

--        buildGrouped grouped = addNodeHandleFlagsWith $
--            State.setGrouped (grouped ^. Label.label . to Enum.id) >> buildNode astFolded monadicBind outName grouped

--        buildVar (Expr.Variable name _) = do
--            isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp nodeID
--            if astFolded && isBound
--                then return nodeID
--                else addExprNode (toString name) []

--        --buildApp src args = addNodeHandleFlagsWith $ do
--        --    srcID <- buildNode astFolded False outName src
--        --    State.gvmNodeMapLookUp srcID >>= \case
--        --       Just (srcNID, _) -> buildAndConnectMany True True Nothing srcNID (fmap (view Arg.arg) args) 1
--        --       Nothing          -> return ()
--        --    connectMonadic srcID
--        --    return srcID

--        addNodeHandleFlags = addNodeHandleFlagsWith .: addNode nodeID

--        addNodeHandleFlagsWith action = do
--            graphFolded <- State.getGraphFolded nodeID
--            let minID = MinID.run lexpr
--            generated   <- State.getDefaultGenerated minID
--            if graphFolded
--                then addNode minID (mkNodeStrExpr lexpr) []
--                else if generated
--                    then addNode minID (mkNodeAstExpr lexpr) []
--                    else action

--        addNode i nodeExpr args = do
--            let node = Node.Expr nodeExpr (genName nodeExpr i)
--            State.addNode i Port.All node astFolded assignment
--            buildAndConnectMany True True Nothing i args 0
--            connectMonadic i
--            return i

--        addExprNode name   = addNodeHandleFlags (NodeExpr.StringExpr $ StringExpr.fromString name)
--        showAndAddNode     = addNodeHandleFlags (mkNodeStrExpr lexpr) []
--        connectMonadic     = when monadicBind . State.connectMonadic

--        mkNodeAstExpr      = NodeExpr.ASTExpr
--        mkNodeStrExpr      = NodeExpr.StringExpr . StringExpr.fromString . lunaShow
--        assignment         = Maybe.isJust outName
--        genName nodeExpr i = Maybe.fromMaybe (OutputName.generate nodeExpr i) outName


--isNativeVar (Expr.NativeVar {}) = True
--isNativeVar _                   = False


--buildArg :: Bool -> Bool -> Maybe String -> Expr -> GBPass (Maybe AST.ID)
--buildArg astFolded monadicBind outName lexpr = case lexpr of
--    Expr.Wildcard _ -> return Nothing
--    _               -> Just <$> buildNode astFolded monadicBind outName lexpr


--buildAndConnectMany :: Bool -> Bool -> Maybe String -> AST.ID -> [LExpr a v] -> Int -> GBPass a V m ()
--buildAndConnectMany astFolded monadicBind outName dstID lexprs start =
--    mapM_ (buildAndConnect astFolded monadicBind outName dstID) $ zip lexprs $ map Port.Num [start..]


--buildAndConnect :: Bool -> Bool -> Maybe String -> AST.ID -> (LExpr a V, Port) -> GBPass a V m ()
--buildAndConnect astFolded monadicBind outName dstID (lexpr, dstPort) = do
--    msrcID <- buildArg astFolded monadicBind outName lexpr
--    case msrcID of
--        Nothing    -> return ()
--        Just srcID -> State.connect srcID dstID dstPort


--isRealPat :: Pat -> Expr -> GBPass Bool
--isRealPat pat dst = do
--    isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp (dst ^. Expr.id)
--    return $ case (pat, dst, isBound) of
--        (Pat.Var {}, Expr.Var {}, True) -> True
--        (Pat.Var {}, _          , _   ) -> False
--        _                               -> True


--buildPat :: (Enumerated ae, Enumerated ad) => LPat ad -> GBPass V m [AST.ID]
--buildPat p = case unwrap p of
--    Pat.Var      _      -> return [i]
--    Pat.Lit      _      -> return [i]
--    Pat.Tuple    items  -> List.concat <$> mapM buildPat items
--    Pat.Con      _      -> return [i]
--    Pat.App      _ args -> List.concat <$> mapM buildPat args
--    Pat.Typed    pat _  -> buildPat pat
--    Pat.Wildcard        -> return [i]
--    Pat.Grouped  pat    -> buildPat pat
--    where i = p ^. Label.label . to Enum.id

----showNative :: Expr -> String
----showNative native = case native of
----    Expr.Native       _ segments     -> "```" ++ concatMap showNative segments ++ "```"
----    Expr.NativeCode   _ code         -> code
----    Expr.NativeVar    _ _            -> "#{}"
----    _                                -> Prelude.error $ "Graph.Builder.Builder.showNative: Not a native: " ++ show native
