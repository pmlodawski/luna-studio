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
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

module Luna.Pass.Transform.Graph.Builder.Builder where

import           Control.Monad.State                      hiding (mapM, mapM_)
import           Control.Monad.Trans.Either
import qualified Data.Maybe                               as Maybe

import           Flowbox.Prelude
import           Flowbox.System.Log.Logger                hiding (error)
import           Luna.Data.StructInfo                     (StructInfo)
import qualified Luna.Pass.Analysis.Find.Find             as Find
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
import qualified Luna.Syntax.Graph.Tag                    as Tag
import           Luna.Syntax.Label                        (Label (Label))
import qualified Luna.Syntax.Label                        as Label
import           Luna.Syntax.Lit                          (LLit)
import qualified Luna.Syntax.Name.Pattern                 as Pattern
import           Luna.Util.LunaShow                       (LunaShow, lunaShow)


logger :: LoggerIO
logger = getLoggerIO $moduleName


type LunaExpr ae = (Enumerated ae, LunaShow (LExpr ae V), LunaShow (LLit ae))
type V = ()


run :: Monad m => StructInfo -> TDecl V -> EitherT State.Error m (TDecl V, Graph Tag V)
run aliasInfo lexpr = evalStateT (func2graph lexpr) $ State.mk aliasInfo Node.inputsID


func2graph :: TDecl V -> GBPass V m (TDecl V, Graph Tag V)
func2graph decl_ = do
    (inputsPos, outputPos, decl) <- State.initFreeNodeID decl_
    let (Label _ (Decl.Func (Decl.FuncDecl _ sig _ body))) = decl
    State.insNode (Node.inputsID, Node.Inputs inputsPos)
    State.insNode (Node.outputID, Node.Outputs def outputPos)
    sig'  <- buildInputsArgs sig
    body' <- buildBody body
    decl' <- State.saveFreeNodeID =<< State.connectMonadicOutput decl
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
            lift $ State.addToNodeMap (Enum.id $ pat ^. Label.label) (Node.inputsID, Port.mkSrc no)
            return arg


buildBody :: [TExpr V] -> GBPass V m [TExpr V]
buildBody []   = return []
buildBody body = do
    body' <- mapM (fmap (view _1) . buildNode Nothing []) (init body)
    let output = last body
    output' <- buildOutput output
    return $ body' ++ [output']


buildOutput :: TExpr V -> GBPass V m (TExpr V)
buildOutput lexpr = case unwrap lexpr of
    Expr.Assignment {}                        -> view _1 <$> buildNode Nothing [] lexpr
    Expr.Grouped (Label l (Expr.Tuple items)) -> do
        (items', argRefs) <- processArgs items
        let defaults = ArgRef.defaults argRefs
            nodes    = ArgRef.nodes    argRefs
        mapM_ (\(srcID, srcPort, dstPort) -> State.connect srcID srcPort Node.outputID dstPort) nodes
        return $ Label tag $ Expr.Grouped $ Label l $ Expr.Tuple items'
    --Expr.Tuple   items                        -> buildAndConnectMany True  True Nothing Node.outputID items 0
    --Expr.Grouped (Label _ (Expr.Tuple items)) -> buildAndConnectMany True  True Nothing Node.outputID items 0
    --Expr.Grouped v@(Label _ (Expr.Var {}))    -> buildAndConnect     True  True Nothing Node.outputID (v, Port.Num 0)
    --Expr.Grouped V                            -> buildAndConnect     False True Nothing Node.outputID (v, Port.Num 0)
    --Expr.Var {}                               -> buildAndConnect     True  True Nothing Node.outputID (lexpr, Port.All)
    --_                                         -> buildAndConnect     False True Nothing Node.outputID (lexpr, Port.All)
    _   ->  do (lexpr', nodeID, srcPort) <- buildNode Nothing [] lexpr
               State.connect nodeID srcPort Node.outputID Port.mkDstAll
               return lexpr'
    where tag = lexpr ^. Label.label


buildExprApp :: Expr.ExprApp Tag V -> GBPass V m (Expr.ExprApp Tag V, [ArgRef])
buildExprApp (Pattern.NamePat prefix base segmentList) = fmap (_2 %~ Maybe.catMaybes) . flip runStateT [] $ do
    (prefix', base') <- buildBase prefix base
    segmentList' <- mapM buildSegment segmentList
    modify reverse
    return $ Pattern.NamePat prefix' base' segmentList'
    where
        addArg arg = modify (arg:)

        buildBase prefix_ (Pattern.Segment lexpr sArgs) = do
            lexpr' <- case lexpr of
                Label l (Expr.Accessor accName src) -> do port <- Port.mkDst . length <$> get
                                                          (src', arg) <- lift $ processArg (src, port)
                                                          addArg arg
                                                          return $ Label l (Expr.Accessor accName src')
                Label _ (Expr.Cons _) -> addArg Nothing >> return lexpr
                Label _ (Expr.Var  _) -> addArg Nothing >> return lexpr
            prefix' <- buildPrefix prefix_
            (prefix',) . Pattern.Segment lexpr' <$> mapM buildAppArg sArgs

        buildPrefix = \case
            Nothing     -> return Nothing
            Just appArg -> Just <$> buildAppArg appArg

        buildSegment (Pattern.Segment sBase sArgs) =
            Pattern.Segment sBase <$> mapM buildAppArg sArgs

        buildAppArg (Expr.AppArg argName e) = do
            port <- Port.mkDst . length <$> get
            (e', arg) <- lift $ processArg (e, port)
            addArg arg
            return $ Expr.AppArg argName e'


buildNode :: Maybe TPat -> [Tag] -> TExpr V -> GBPass V m (TExpr V, Node.ID, SrcPort)
buildNode outputName groupInfo lexpr = if tag ^. Tag.folded
    then showAndAddNode
    else case unwrap lexpr of
        Expr.Assignment dst src -> do
            (src', nodeID, srcPort) <- buildNode (Just dst) groupInfo src
            State.registerIDs lexpr (nodeID, Port.mkSrcAll)
            return (Label tag $ Expr.Assignment dst src', nodeID, srcPort)
        Expr.Grouped grouped -> (_1 %~ Label tag . Expr.Grouped) <$>  buildNode outputName (lexpr ^. Label.label :groupInfo) grouped
        Expr.Tuple items -> do
            (items', argRefs) <- processArgs items
            let lexpr' = Label tag $ Expr.Tuple items'
            (le, ni)  <- addNodeWithExpr outputName (NodeExpr.StringExpr StringExpr.Tuple) argRefs groupInfo lexpr'
            State.registerIDs le (ni, Port.mkSrcAll)
            return (le, ni, Port.mkSrcAll)
        Expr.List (Expr.SeqList items) -> do
            (items', argRefs) <- processArgs items
            let lexpr' = Label tag $ Expr.List $ Expr.SeqList items'
            (le, ni)  <- addNodeWithExpr outputName (NodeExpr.StringExpr StringExpr.List) argRefs groupInfo lexpr'
            State.registerIDs le (ni, Port.mkSrcAll)
            return (le, ni, Port.mkSrcAll)
        Expr.App exprApp -> do
            (exprApp', argRefs) <- buildExprApp exprApp
            let mp     = MultiPart.fromNamePat exprApp'
                lexpr' = Label tag $ Expr.App exprApp'
            (le, ni)  <- addNodeWithExpr outputName (NodeExpr.MultiPart mp) argRefs groupInfo lexpr'
            State.registerIDs le (ni, Port.mkSrcAll)
            return (le, ni, Port.mkSrcAll)
        Expr.Var _ -> State.gvmNodeMapLookUp (Enum.id tag) >>= \case
            Nothing             -> do (le, ni) <- addNode outputName [] groupInfo lexpr
                                      return (le, ni, Port.mkSrcAll)
            Just (srcNID, srcPort) -> if Maybe.isJust outputName
                then do let nodeExpr = NodeExpr.StringExpr $ StringExpr.Pattern ""
                        (le, ni) <- addNodeWithExpr outputName nodeExpr [] groupInfo lexpr
                        State.connect srcNID srcPort ni Port.mkDstAll
                        return (le, ni, Port.mkSrcAll)
                else do State.registerIDs lexpr (srcNID, srcPort)
                        return (lexpr, srcNID, srcPort)
        _ -> showAndAddNode

    where
        tag = lexpr ^. Label.label
        showAndAddNode = do
            (le, ni) <- addNode outputName [] groupInfo lexpr
            State.registerIDs le (ni, Port.mkSrcAll)
            return (le, ni, Port.mkSrcAll)

processArgs :: [TExpr V] -> GBPass V m ([TExpr V], [ArgRef])
processArgs items = do
    itemsArgs <- mapM processArg $ zip items $ map Port.mkDst [0..]
    let items'  = map fst itemsArgs
        argRefs = Maybe.mapMaybe snd itemsArgs
    return (items', argRefs)


processArg :: (TExpr V, DstPort) -> GBPass V m (TExpr V, Maybe ArgRef)
processArg (lexpr, dstPort)
    | constainsVar lexpr = do
        (lexpr', nodeID, srcPort) <- buildNode Nothing [] lexpr
        return (lexpr', Just $ ArgRef.mkNode (nodeID, srcPort, dstPort))
    | isWildcard lexpr = return (lexpr, Nothing)
    | otherwise = return (lexpr, Just $ ArgRef.mkDefault (dstPort, lexpr))


addNode :: Maybe TPat -> [ArgRef] -> [Tag] -> TExpr V -> GBPass V m (TExpr V, Node.ID)
addNode outputName argRefs groupInfo lexpr = addNodeWithExpr outputName nodeExpr argRefs groupInfo lexpr
    where nodeExpr = NodeExpr.StringExpr $ fromString $ lunaShow lexpr


addNodeWithExpr :: Maybe TPat -> NodeExpr Tag V -> [ArgRef] -> [Tag]
                -> TExpr V -> GBPass V m (TExpr V, Node.ID)
addNodeWithExpr outputName nodeExpr argRefs groupInfo lexpr = do
    (nodeID, position, lexpr') <- State.getNodeInfo lexpr
    let defaults = ArgRef.defaults argRefs
        nodes    = ArgRef.nodes    argRefs
        node = Node.Expr nodeExpr outputName (DefaultsMap.fromList defaults) position groupInfo False
    State.insNode (nodeID, node)
    lexpr'' <- State.connectMonadic nodeID lexpr'
    mapM_ (\(srcID, srcPort, dstPort) -> State.connect srcID srcPort nodeID dstPort) nodes
    return (lexpr'', nodeID)


constainsVar :: TExpr V -> Bool
constainsVar = not . null . Find.run isVar where
    isVar (Label _ (Expr.Var {})) = True
    isVar _                       = False

isWildcard :: TExpr V -> Bool
isWildcard (Label _ Expr.Wildcard) = True
isWildcard _                       = False

isAccessorOnWildcard :: TExpr V -> Bool
isAccessorOnWildcard (Label _ (Expr.Accessor _ src)) = isWildcard src
isAccessorOnWildcard _= False
