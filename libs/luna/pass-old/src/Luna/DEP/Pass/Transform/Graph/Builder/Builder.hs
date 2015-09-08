---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.DEP.Pass.Transform.Graph.Builder.Builder where

import           Control.Applicative
import           Control.Monad.State
import           Control.Monad.Trans.Either
import qualified Data.List                  as List
import qualified Data.Maybe                 as Maybe

import           Flowbox.Prelude                             hiding (error, mapM, mapM_)
import qualified Flowbox.Prelude                             as Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.DEP.AST.Arg                            as Arg
import qualified Luna.DEP.AST.Common                         as AST
import           Luna.DEP.AST.Expr                           (Expr)
import qualified Luna.DEP.AST.Expr                           as Expr
import           Luna.DEP.AST.Pat                            (Pat)
import qualified Luna.DEP.AST.Pat                            as Pat
import qualified Luna.DEP.AST.Type                           as Type
import           Luna.DEP.Data.AliasInfo                     (AliasInfo)
import           Luna.DEP.Graph.Graph                        (Graph)
import qualified Luna.DEP.Graph.Node                         as Node
import qualified Luna.DEP.Graph.Node.Expr                    as NodeExpr
import qualified Luna.DEP.Graph.Node.OutputName              as OutputName
import qualified Luna.DEP.Graph.Node.StringExpr              as StringExpr
import           Luna.DEP.Graph.Port                         (Port)
import qualified Luna.DEP.Graph.Port                         as Port
import           Luna.DEP.Graph.PropertyMap                  (PropertyMap)
import qualified Luna.DEP.Pass.Analysis.ID.MinID             as MinID
import qualified Luna.DEP.Pass.Pass                          as Pass
import           Luna.DEP.Pass.Transform.Graph.Builder.State (GBPass)
import qualified Luna.DEP.Pass.Transform.Graph.Builder.State as State
import           Luna.DEP.Util.LunaShow                      (lunaShow)



logger :: LoggerIO
logger = getLoggerIO $moduleName


run :: AliasInfo -> PropertyMap -> Bool -> Expr -> Pass.Result (Graph, PropertyMap)
run aliasInfo pm foldNodes expr = Pass.run_ (Pass.Info "GraphBuilder")
                                  (State.make aliasInfo pm foldNodes inputsID)
                                  (expr2graph expr)
    where inputsID = - expr ^. Expr.id


expr2graph :: Expr -> GBPass (Graph, PropertyMap)
expr2graph expr = case expr of
    Expr.Function i _ _ inputs output body -> processExpr i inputs output body
    Expr.Lambda   i     inputs output body -> processExpr i inputs output body
    _                                      -> left "expr2graph: Unsupported Expr type"

  where
    processExpr i inputs output body = do
        (inputsID, outputID) <- prepareInputsOutputs i (output ^. Type.id)
        parseArgs inputsID inputs
        if null body
            then State.connectMonadic outputID
            else do
                mapM_ (buildNode False True Nothing) $ init body
                buildOutput outputID $ last body
        finalize


prepareInputsOutputs :: AST.ID -> AST.ID -> GBPass (Node.ID, Node.ID)
prepareInputsOutputs functionID funOutputID = do
    let inputsID = - functionID
        outputID = - funOutputID
    State.insNode (inputsID, Node.Inputs)
    State.insNode (outputID, Node.Outputs)
    return (inputsID, outputID)


finalize :: GBPass (Graph, PropertyMap)
finalize = do g  <- State.getGraph
              pm <- State.getPropertyMap
              return (g, pm)


parseArgs :: Node.ID -> [Expr] -> GBPass ()
parseArgs inputsID inputs = do
    let numberedInputs = zip inputs [0..]
    mapM_ (parseArg inputsID) numberedInputs


parseArg :: Node.ID -> (Expr, Int) -> GBPass ()
parseArg inputsID (input, no) = case input of
    Expr.Arg _ pat _ -> do [p] <- buildPat pat
                           State.addToNodeMap p (inputsID, Port.Num no)
    _                -> left "parseArg: Wrong Expr type"


buildOutput :: Node.ID -> Expr -> GBPass ()
buildOutput outputID expr = do
    case expr of
        Expr.Assignment {}                  -> void $ buildNode    False True Nothing expr
        Expr.Tuple   _ items                -> buildAndConnectMany True  True Nothing outputID items 0
        Expr.Grouped _ (Expr.Tuple _ items) -> buildAndConnectMany True  True Nothing outputID items 0
        Expr.Grouped _ (v@Expr.Var {})      -> buildAndConnect     True  True Nothing outputID (v, Port.Num 0)
        Expr.Grouped _  v                   -> buildAndConnect     False True Nothing outputID (v, Port.Num 0)
        Expr.Var {}                         -> buildAndConnect     True  True Nothing outputID (expr, Port.All)
        _                                   -> buildAndConnect     False True Nothing outputID (expr, Port.All)
    State.connectMonadic outputID


buildNode :: Bool -> Bool -> Maybe String -> Expr -> GBPass AST.ID
buildNode astFolded monadicBind outName expr = case expr of
    Expr.Assignment _ pat dst               -> buildAssignment pat dst
    Expr.App        _ src args              -> buildApp src args
    Expr.Accessor   _ acc dst               -> addExprNode (view Expr.accName acc) [dst]
    Expr.Infix      _ name src dst          -> addExprNode name [src, dst]
    Expr.Var        _ name                  -> buildVar name
    Expr.NativeVar  _ name                  -> buildVar name
    Expr.Con        _ name                  -> addExprNode name []
    Expr.Lit        _ lvalue                -> addExprNode (lunaShow lvalue) []
    Expr.Tuple      _ items                 -> addNodeHandleFlags (NodeExpr.StringExpr StringExpr.Tuple) items
    Expr.List       _ [Expr.RangeFromTo {}] -> showAndAddNode
    Expr.List       _ [Expr.RangeFrom   {}] -> showAndAddNode
    Expr.List       _ items                 -> addNodeHandleFlags (NodeExpr.StringExpr StringExpr.List) items
    Expr.Native     _ segments              -> addNodeHandleFlags (NodeExpr.StringExpr $ StringExpr.Native $ showNative expr) $ filter isNativeVar segments
    Expr.Wildcard   _                       -> left $ "GraphBuilder.buildNode: Unexpected Expr.Wildcard with id=" ++ show nodeID
    Expr.Grouped    _ grouped               -> buildGrouped grouped
    _                                       -> showAndAddNode
    where
        nodeID = getID expr where
            getID (Expr.App _ src _) = getID src
            getID e                  = e ^. Expr.id

        buildAssignment pat dst = do
            let patStr = lunaShow pat
            realPat <- isRealPat pat dst
            if realPat
                then do patIDs <- buildPat pat
                        let nodeExpr = NodeExpr.StringExpr $ StringExpr.Pattern patStr
                            node     = Node.Expr nodeExpr (genName nodeExpr nodeID)
                        State.insNodeWithFlags (nodeID, node) astFolded assignment
                        case patIDs of
                           [patID] -> State.addToNodeMap patID (nodeID, Port.All)
                           _       -> mapM_ (\(n, patID) -> State.addToNodeMap patID (nodeID, Port.Num n)) $ zip [0..] patIDs
                        dstID <- buildNode True True Nothing dst
                        State.connect dstID nodeID $ Port.Num 0
                        connectMonadic nodeID
                        return nodeID
                else do [p] <- buildPat pat
                        j <- buildNode False True (Just patStr) dst
                        State.addToNodeMap p (j, Port.All)
                        return j

        buildGrouped grouped = addNodeHandleFlagsWith $
            State.setGrouped (grouped ^. Expr.id) >> buildNode astFolded monadicBind outName grouped

        buildVar name = do
            isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp nodeID
            if astFolded && isBound
                then return nodeID
                else addExprNode name []

        buildApp src args = addNodeHandleFlagsWith $ do
            srcID <- buildNode astFolded False outName src
            State.gvmNodeMapLookUp srcID >>= \case
               Just (srcNID, _) -> buildAndConnectMany True True Nothing srcNID (fmap (view Arg.arg) args) 1
               Nothing          -> return ()
            connectMonadic srcID
            return srcID

        addNodeHandleFlags = addNodeHandleFlagsWith .: addNode nodeID

        addNodeHandleFlagsWith action = do
            graphFolded <- State.getGraphFolded nodeID
            minID  <- hoistEither =<< MinID.runExpr expr
            generated   <- State.getDefaultGenerated minID
            if graphFolded
                then addNode minID (mkNodeStrExpr expr) []
                else if generated
                    then addNode minID (mkNodeAstExpr expr) []
                    else action

        addNode i nodeExpr args = do
            let node = Node.Expr nodeExpr (genName nodeExpr i)
            State.addNode i Port.All node astFolded assignment
            buildAndConnectMany True True Nothing i args 0
            connectMonadic i
            return i

        addExprNode name   = addNodeHandleFlags (NodeExpr.StringExpr $ StringExpr.fromString name)
        showAndAddNode     = addNodeHandleFlags (mkNodeStrExpr expr) []
        connectMonadic     = when monadicBind . State.connectMonadic

        mkNodeAstExpr      = NodeExpr.ASTExpr
        mkNodeStrExpr      = NodeExpr.StringExpr . StringExpr.fromString . lunaShow
        assignment         = Maybe.isJust outName
        genName nodeExpr i = Maybe.fromMaybe (OutputName.generate nodeExpr i) outName
        isNativeVar (Expr.NativeVar {}) = True
        isNativeVar _                   = False


buildArg :: Bool -> Bool -> Maybe String -> Expr -> GBPass (Maybe AST.ID)
buildArg astFolded monadicBind outName expr = case expr of
    Expr.Wildcard _ -> return Nothing
    _               -> Just <$> buildNode astFolded monadicBind outName expr


buildAndConnectMany :: Bool -> Bool -> Maybe String -> AST.ID -> [Expr] -> Int ->  GBPass ()
buildAndConnectMany astFolded monadicBind outName dstID exprs start =
    mapM_ (buildAndConnect astFolded monadicBind outName dstID) $ zip exprs $ map Port.Num [start..]


buildAndConnect :: Bool -> Bool -> Maybe String -> AST.ID -> (Expr, Port) -> GBPass ()
buildAndConnect astFolded monadicBind outName dstID (expr, dstPort) = do
    msrcID <- buildArg astFolded monadicBind outName expr
    case msrcID of
        Nothing    -> return ()
        Just srcID -> State.connect srcID dstID dstPort


isRealPat :: Pat -> Expr -> GBPass Bool
isRealPat pat dst = do
    isBound <- Maybe.isJust <$> State.gvmNodeMapLookUp (dst ^. Expr.id)
    return $ case (pat, dst, isBound) of
        (Pat.Var {}, Expr.Var {}, True) -> True
        (Pat.Var {}, _          , _   ) -> False
        _                               -> True


buildPat :: Pat -> GBPass [AST.ID]
buildPat p = case p of
    Pat.Var      i _      -> return [i]
    Pat.Lit      i _      -> return [i]
    Pat.Tuple    _ items  -> List.concat <$> mapM buildPat items
    Pat.Con      i _      -> return [i]
    Pat.App      _ _ args -> List.concat <$> mapM buildPat args
    Pat.Typed    _ pat _  -> buildPat pat
    Pat.Wildcard i        -> return [i]
    Pat.Grouped  _ pat    -> buildPat pat


showNative :: Expr -> String
showNative native = case native of
    Expr.Native       _ segments     -> "```" ++ concatMap showNative segments ++ "```"
    Expr.NativeCode   _ code         -> code
    Expr.NativeVar    _ _            -> "#{}"
    _                                -> Prelude.error $ "Graph.Builder.Builder.showNative: Not a native: " ++ show native
