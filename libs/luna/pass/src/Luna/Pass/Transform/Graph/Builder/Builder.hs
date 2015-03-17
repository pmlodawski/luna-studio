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

--import           Control.Applicative
import Control.Monad.State
import Control.Monad.Trans.Either
import Data.Either                (lefts, rights)

--import qualified Luna.Pass.Analysis.ID.MinID             as MinID
--import qualified Luna.Syntax.Arg                         as Arg
--import qualified Luna.Syntax.Graph.Node.OutputName       as OutputName
--import qualified Luna.Syntax.Type                        as Type
--import           Luna.Syntax.Pat                         (LPat)
--import qualified Luna.Syntax.Pat                         as Pat
--import           Luna.Syntax.Traversals.Class            (Traversal)
--import qualified Luna.Syntax.Graph.Tag                   as Tag
--import qualified Luna.Syntax.Enum                        as Enum
--import           Luna.Syntax.Decl                        (LDecl)
import           Flowbox.Prelude                         hiding (Traversal, error, mapM, mapM_)
import           Flowbox.System.Log.Logger
import           Luna.Data.StructInfo                    (StructInfo)
import           Luna.Pass.Transform.Graph.Builder.State (GBPass)
import qualified Luna.Pass.Transform.Graph.Builder.State as State
import           Luna.Syntax.Arg                         (Arg (Arg))
import qualified Luna.Syntax.Decl                        as Decl
import           Luna.Syntax.Enum                        (Enumerated)
import           Luna.Syntax.Expr                        (LExpr)
import qualified Luna.Syntax.Expr                        as Expr
import qualified Luna.Syntax.Graph.DefaultsMap           as DefaultsMap
import           Luna.Syntax.Graph.Graph                 (Graph)
import qualified Luna.Syntax.Graph.Node                  as Node
import           Luna.Syntax.Graph.Node.Expr             (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr             as NodeExpr
import qualified Luna.Syntax.Graph.Node.StringExpr       as StringExpr
import           Luna.Syntax.Graph.Port                  (Port)
import qualified Luna.Syntax.Graph.Port                  as Port
import           Luna.Syntax.Graph.PortDescriptor        (PortDescriptor)
import           Luna.Syntax.Graph.Tag                   (TDecl, TExpr, TPat, Tag)
import           Luna.Syntax.Label                       (Label (Label))
import qualified Luna.Syntax.Label                       as Label
import           Luna.Syntax.Lit                         (LLit)
import qualified Luna.Syntax.Name.Pattern                as Pattern
import           Luna.Util.LunaShow                      (LunaShow, lunaShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


type LunaExpr ae v = (Enumerated ae, LunaShow (LExpr ae v), LunaShow (LLit ae))


inputsID, outputID :: Node.ID
inputsID = -2
outputID = -1


run :: (Show v, Monad m) => StructInfo -> TDecl v -> EitherT State.Error m (TDecl v, Graph Tag v)
run aliasInfo lexpr = evalStateT (expr2graph lexpr) $ State.mk aliasInfo inputsID


expr2graph :: Show v => TDecl v -> GBPass v m (TDecl v, Graph Tag v)
expr2graph decl@(Label _ (Decl.Func (Decl.FuncDecl _ sig _ body))) = do
    State.initFreeNodeID decl
    State.insNode (inputsID, Node.mkInputs)
    State.insNode (outputID, Node.mkOutputs)
    sig'  <- buildArgs sig
    body' <- buildBody body
    decl' <- State.saveFreeNodeID decl
    (,) <$> return (decl' & Label.element . Decl.funcDecl . Decl.funcDeclSig  .~ sig'
                          & Label.element . Decl.funcDecl . Decl.funcDeclBody .~ body')
        <*> State.getGraph


buildArgs :: Decl.FuncSig Tag e -> GBPass v m (Decl.FuncSig Tag e)
buildArgs (Pattern.NamePat prefix base segmentList) = flip evalStateT (0::Int) $ do
    prefix' <- case prefix of
        Nothing -> return Nothing
        Just pr -> Just <$> buildArg pr
    base'        <- parseSegment base
    segmentList' <- mapM parseSegment segmentList
    return $ Pattern.NamePat prefix' base' segmentList'


parseSegment (Pattern.Segment base args) =
    Pattern.Segment base <$> mapM buildArg args


buildArg (Arg pat val) = do
    no <- get
    put $ no + 1
    (i, _, pat') <- lift $ State.getNodeInfo pat
    lift $ State.addToNodeMap i (inputsID, Port.Num no)
    return $ Arg pat' val


buildBody :: Show v => [TExpr v] -> GBPass v m [TExpr v]
buildBody []   = State.connectMonadic outputID >> return []
buildBody body = do
    body' <- mapM (flip buildNode Nothing) (init body)
    let output' = last body
    output' <- buildOutput outputID $ last body
    return $ body' ++ [output']


buildOutput :: Show v => Node.ID -> TExpr v -> GBPass v m (TExpr v)
buildOutput outputID lexpr = do
    lexpr' <- case unwrap lexpr of
        Expr.Assignment {}                        -> buildNode lexpr Nothing
        --Expr.Tuple   items                        -> buildAndConnectMany True  True Nothing outputID items 0
        --Expr.Grouped (Label _ (Expr.Tuple items)) -> buildAndConnectMany True  True Nothing outputID items 0
        --Expr.Grouped v@(Label _ (Expr.Var {}))    -> buildAndConnect     True  True Nothing outputID (v, Port.Num 0)
        --Expr.Grouped v                            -> buildAndConnect     False True Nothing outputID (v, Port.Num 0)
        --Expr.Var {}                               -> buildAndConnect     True  True Nothing outputID (lexpr, Port.All)
        --_                                         -> buildAndConnect     False True Nothing outputID (lexpr, Port.All)
        _   ->  do lexpr' <- buildNode lexpr Nothing -- FIXME FIXME FIXME FIXME FIXME FIXME
                   (nodeID, _, lexpr'') <- State.getNodeInfo lexpr'
                   State.connect nodeID outputID Port.All
                   return lexpr''
    State.connectMonadic outputID
    return lexpr'



buildNode :: Show v => TExpr v -> Maybe TPat -> GBPass v m (TExpr v)
buildNode lexpr outputName = case unwrap lexpr of
    Expr.Assignment dst src   -> do
        src' <- buildNode src (Just dst)
        return $ Label tag $ Expr.Assignment dst src'
    Expr.Tuple items  -> do
        itemsArgs <- mapM processArg $ zip items $ map Port.Num [0..]
        let items'   = map fst itemsArgs
            defaults = lefts $ map snd itemsArgs
            args     = rights $ map snd itemsArgs
            lexpr' = Label tag $ Expr.Tuple items'
        addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.Tuple) args defaults
    Expr.List  (Expr.SeqList items)  -> do
        itemsArgs <- mapM processArg $ zip items $ map Port.Num [0..]
        let items'   = map fst itemsArgs
            defaults = lefts $ map snd itemsArgs
            args     = rights $ map snd itemsArgs
            lexpr' = Label tag $ Expr.List $ Expr.SeqList items'
        addNodeWithExpr lexpr' outputName (NodeExpr.StringExpr StringExpr.List) args defaults
    --Expr.App exprApp -> addNodeWithArgs

    _ -> addNode lexpr outputName [] []

    --Expr.Var (Expr.Variable vname _) -> do
    --        isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp nodeID
    --        if isBound
    --            then return nodeID
    --            else addExprNode (toString name) []
    where
        tag = lexpr ^. Label.label


processArg :: Show v => (TExpr v, Port) -> GBPass v m (TExpr v, Either (PortDescriptor, NodeExpr Tag v) (Node.ID, Port))
processArg (lexpr, port) = if undefined --constainsVar
    then do (nodeID, _, lexpr') <- State.getNodeInfo =<< buildNode lexpr (Just undefined)
            return (lexpr', Right (nodeID, port))
    else return (lexpr, Left (Port.toList port, NodeExpr.StringExpr $ StringExpr.fromString $ lunaShow lexpr))


addNode :: Show v => TExpr v -> Maybe TPat -> [(Node.ID, Port)] -> [(PortDescriptor, NodeExpr Tag v)] -> GBPass v m (TExpr v)
addNode lexpr outputName args defaults = addNodeWithExpr lexpr outputName nodeExpr args defaults
    where nodeExpr = NodeExpr.StringExpr $ StringExpr.fromString $ lunaShow lexpr


addNodeWithExpr :: TExpr v -> Maybe TPat
                -> NodeExpr Tag v -> [(Node.ID, Port)] -> [(PortDescriptor, NodeExpr Tag v)] -> GBPass v m (TExpr v)
addNodeWithExpr lexpr outputName nodeExpr args defaults = do
    (nodeID, position, lexpr') <- State.getNodeInfo lexpr
    let node = Node.Expr nodeExpr outputName (DefaultsMap.fromList defaults) position
    State.insNode (nodeID, node)
    State.addToNodeMap nodeID (nodeID, Port.All)
    mapM_ (\(srcID, port) -> State.connect srcID nodeID port) args
    return lexpr'



--buildNode :: LunaExpr a v
--          => Bool -> Bool -> Maybe VNameP -> LExpr a v -> GBPass a v m AST.ID
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


--buildAndConnectMany :: Bool -> Bool -> Maybe String -> AST.ID -> [LExpr a v] -> Int -> GBPass a v m ()
--buildAndConnectMany astFolded monadicBind outName dstID lexprs start =
--    mapM_ (buildAndConnect astFolded monadicBind outName dstID) $ zip lexprs $ map Port.Num [start..]


--buildAndConnect :: Bool -> Bool -> Maybe String -> AST.ID -> (LExpr a v, Port) -> GBPass a v m ()
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


--buildPat :: (Enumerated ae, Enumerated ad) => LPat ad -> GBPass v m [AST.ID]
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
