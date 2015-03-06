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
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe

import Flowbox.Prelude hiding (Traversal, error, mapM, mapM_)
--import qualified Flowbox.Prelude                         as Prelude
import Flowbox.System.Log.Logger
import Luna.Data.StructInfo      (StructInfo)
--import qualified Luna.Pass.Analysis.ID.MinID             as MinID
import           Luna.Pass.Analysis.ID.State             (IDState)
import qualified Luna.Pass.Pass                          as Pass
import           Luna.Pass.Transform.Graph.Builder.State (GBPass)
import qualified Luna.Pass.Transform.Graph.Builder.State as State
import           Luna.Syntax.Arg                         (Arg (Arg))
--import qualified Luna.Syntax.Arg                         as Arg
import qualified Luna.Syntax.AST         as AST
import           Luna.Syntax.Decl        (LDecl)
import qualified Luna.Syntax.Decl        as Decl
import           Luna.Syntax.Enum        (Enumerated)
import qualified Luna.Syntax.Enum        as Enum
import           Luna.Syntax.Expr        (LExpr)
import qualified Luna.Syntax.Expr        as Expr
import           Luna.Syntax.Graph.Graph (Graph)
import qualified Luna.Syntax.Graph.Node  as Node
--import qualified Luna.Syntax.Graph.Node.Expr             as NodeExpr
--import qualified Luna.Syntax.Graph.Node.OutputName       as OutputName
--import qualified Luna.Syntax.Graph.Node.StringExpr       as StringExpr
import           Luna.Syntax.Graph.Port (Port)
import qualified Luna.Syntax.Graph.Port as Port
import           Luna.Syntax.Label      (Label (Label))
import qualified Luna.Syntax.Label      as Label
import           Luna.Syntax.Lit        (LLit)
--import           Luna.Syntax.Name                        (VNameP)
import qualified Luna.Syntax.Name.Pattern     as Pattern
import           Luna.Syntax.Pat              (LPat)
import qualified Luna.Syntax.Pat              as Pat
import           Luna.Syntax.Traversals.Class (Traversal)
--import qualified Luna.Syntax.Type                        as Type
import           Luna.Syntax.Graph.Tag    (TDecl, TExpr, Tag)
import qualified Luna.Syntax.Graph.Tag    as Tag
import           Luna.System.Pragma.Store (MonadPragmaStore)
import           Luna.Util.LunaShow       (LunaShow, lunaShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


type LunaExpr ae v = (Enumerated ae, LunaShow (LExpr ae v), LunaShow (LLit ae))


inputsID = -1
outputID = -2


run :: (MonadPragmaStore m, LunaExpr ae v)
    => StructInfo -> Bool -> TDecl v
    -> EitherT Pass.PassError m (TDecl v, Graph ae v)
run aliasInfo foldNodes lexpr =
    Pass.run_ (Pass.Info "GraphBuilder")
        (State.make aliasInfo foldNodes inputsID)
        (expr2graph lexpr)


expr2graph :: LunaExpr ae v
           => TDecl v -> GBPass ae v m (TDecl v, Graph ae v)
expr2graph decl@(Label l (Decl.Func (Decl.FuncDecl _ sig output body))) = do
    State.initFreeNodeID decl
    State.insNode (inputsID, Node.Inputs)
    State.insNode (outputID, Node.Outputs)
    sig'  <- buildArgs sig
    body' <- buildBody body
    decl' <- State.saveFreeNodeID decl
    (,) <$> return (decl' & Label.element . Decl.funcDecl . Decl.funcDeclSig  .~ sig'
                          & Label.element . Decl.funcDecl . Decl.funcDeclBody .~ body')
        <*> State.getGraph


buildArgs :: Enumerated ae => Decl.FuncSig Tag e -> GBPass ae v m (Decl.FuncSig Tag e)
buildArgs inputs@(Pattern.NamePat prefix base segmentList) = flip evalStateT (0::Int) $ do
    prefix' <- case prefix of
        Nothing -> return Nothing
        Just pr -> Just <$> buildArg pr
    base'        <- parseSegment base
    segmentList' <- mapM parseSegment segmentList
    return $ Pattern.NamePat prefix base segmentList'


parseSegment (Pattern.Segment base args) =
    Pattern.Segment base <$> mapM buildArg args


buildArg (Arg pat val) = do
    no <- get
    put $ no + 1
    (i, _, pat') <- lift $ State.getNodeInfo pat
    lift $ State.addToNodeMap i (inputsID, Port.Num no)
    return $ Arg pat' val


buildBody :: [TExpr v] -> GBPass ae v m [TExpr v]
buildBody []   = State.connectMonadic outputID >> return []
buildBody body = do
    body' <- mapM (flip buildNode Nothing) (init body)
    --mapM_ (buildNode False True Nothing) $ init body
    let output' = last body
    --output' <- buildOutput outputID $ last body
    return $ body' ++ [output']




--buildOutput :: LunaExpr a v
--            => Node.ID -> LExpr a v -> GBPass a v m ()
--buildOutput outputID lexpr = do
--    case unwrap lexpr of
--        Expr.Assignment {}                        -> void $ buildNode    False True Nothing lexpr
--        Expr.Tuple   items                        -> buildAndConnectMany True  True Nothing outputID items 0
--        Expr.Grouped (Label _ (Expr.Tuple items)) -> buildAndConnectMany True  True Nothing outputID items 0
--        Expr.Grouped v@(Label _ (Expr.Var {}))    -> buildAndConnect     True  True Nothing outputID (v, Port.Num 0)
--        Expr.Grouped v                            -> buildAndConnect     False True Nothing outputID (v, Port.Num 0)
--        Expr.Var {}                               -> buildAndConnect     True  True Nothing outputID (lexpr, Port.All)
--        _                                         -> buildAndConnect     False True Nothing outputID (lexpr, Port.All)
--    State.connectMonadic outputID


----FIXME[PM]: remove
--processExpr     = undefined
--buildArg        = undefined
--showNative      = undefined
--isRealPat       = undefined
--isNativeVar     = undefined
--buildApp        = undefined
--buildAssignment = undefined
-------------------

buildNode lexpr outputName = case unwrap lexpr of
    Expr.Assignment dst src   -> do src' <- buildNode src (Just undefined)
                                    return $ Label tag $ Expr.Assignment dst src'
    Expr.Lit lit              -> do (nodeID, position, lexpr') <- State.getNodeInfo lexpr
                                    undefined


    --Expr.App exprApp
    where
        tag   = lexpr ^. Label.label



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


--buildPat :: (Enumerated ae, Enumerated ad) => LPat ad -> GBPass ae v m [AST.ID]
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
