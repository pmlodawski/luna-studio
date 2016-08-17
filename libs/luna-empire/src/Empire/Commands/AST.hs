{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.Commands.AST where

import           Control.Monad.Error                     (throwError)
import           Control.Monad.Except                    (runExceptT)
import           Control.Monad.State
import           Data.Graph                              (Inputs (..), source)
import           Data.HMap.Lazy                          (TypeKey (..))
import qualified Data.HMap.Lazy                          as HMap
import           Data.Layer_OLD.Cover_OLD                (covered, uncover)
import           Data.Maybe                              (maybeToList)
import           Data.Prop                               (prop, ( # ))
import           Data.Record                             (ANY (..), caseTest, of')
import           GHC.Prim                                (Any)
import           Prologue                                hiding (( # ))

import           Empire.API.Data.DefaultValue            (PortDefault, Value (..))
import qualified Empire.API.Data.Error                   as APIError
import           Empire.API.Data.Node                    (NodeId)
import           Empire.API.Data.NodeMeta                (NodeMeta)
import           Empire.API.Data.TypeRep                 (TypeRep)
import           Empire.Data.AST                         (AST, ASTNode, NodeRef)
import           Empire.Data.NodeMarker                  (NodeMarker (..))
import           Empire.Empire

import           Empire.ASTOp                            (ASTOp, runASTOp)
import qualified Empire.ASTOps.Builder                   as ASTBuilder
import qualified Empire.ASTOps.Parse                     as Parser
import qualified Empire.ASTOps.Print                     as Printer
import           Empire.ASTOps.Remove                    (safeRemove)

import           Luna.Pretty.GraphViz                    (renderAndOpen)

import           Luna.Syntax.Model.Network.Builder       (Meta (..), TCData (..), Type (..), tcErrors)
import qualified Luna.Syntax.Model.Network.Builder       as Builder
import           Old.Luna.Syntax.Term.Class              (Acc (..), App (..), Cons (..), Unify (..), Var (..))
import qualified Old.Luna.Syntax.Term.Expr.Lit           as Lit

import           Luna.Compilation.Error                  as TCError
import           Luna.Compilation.Pass.Interpreter.Layer (InterpreterData (..))
import           Luna.Compilation.Pass.Interpreter.Value (toIO, unsafeFromData)
import qualified Luna.Compilation.Pass.Interpreter.Layer as Interpreter
import           Unsafe.Coerce

import           Empire.Commands.Graphics                (fromFigure, fromGeoComponent, fromGeometry, fromGraphics, fromLayer, fromMaterial,
                                                          fromPrimitive, fromShape, fromSurface)

import           Debug.Trace                             (trace)


-- TODO: This might deserve rewriting to some more general solution
import           Luna.Compilation.Pass.Interpreter.Charts (autoScatterChartInt, autoScatterChartDouble, autoScatterChartIntTuple, autoScatterChartDoubleTuple)
import           Graphics.API                             (Material(SolidColor), Figure(..))

metaKey :: TypeKey NodeMeta
metaKey = TypeKey

addNode :: NodeId -> String -> String -> Command AST (NodeRef)
addNode nid name expr = runASTOp $ Parser.parseExpr expr >>= ASTBuilder.makeNodeRep (NodeMarker nid) name

addDefault :: PortDefault -> Command AST (NodeRef)
addDefault val = runASTOp $ Parser.parsePortDefault val

getNodeValueReprs :: NodeRef -> Command AST [Value]
getNodeValueReprs ref = do
    nodeValue <- getNodeValue ref
    return $ case nodeValue of
        Nothing  -> []
        Just val -> case val of
            IntList        list -> [val, Graphics $ autoScatterChartInt         gridMat mat figure scale shift list]
            DoubleList     list -> [val, Graphics $ autoScatterChartDouble      gridMat mat figure scale shift list]
            Histogram      list -> [val, Graphics $ autoScatterChartIntTuple    gridMat mat figure scale shift list]
            IntPairList    list -> [val, Graphics $ autoScatterChartIntTuple    gridMat mat figure scale shift list]
            DoublePairList list -> [val, Graphics $ autoScatterChartDoubleTuple gridMat mat figure scale shift list]
            otherwise -> [val]
            where
                gridMat = SolidColor 0.25 0.25 0.25 1.0
                mat     = SolidColor 0.2  0.5  0.7  1.0
                figure  = Circle 0.016
                scale   = 0.84
                shift   = 0.05

getNodeValue :: NodeRef -> Command AST (Maybe Value)
getNodeValue ref = runASTOp $ do
    node   <- Builder.read ref
    tp     <- Builder.follow source $ node ^. prop Type
    tpNode <- Builder.read tp
    case (node ^. prop InterpreterData . Interpreter.value) of
        Left  err -> trace (show err) $ return Nothing
        Right val -> do
            val <- liftIO . runExceptT $ toIO val
            case val of
                Left _  -> return Nothing
                Right v -> caseTest (uncover tpNode) $ do
                    of' $ \(Cons (Lit.String n) as) -> case n of
                        "Int"            -> return $ Just $ IntValue       $ unsafeFromData v
                        "String"         -> return $ Just $ StringValue    $ unsafeFromData v
                        "Double"         -> return $ Just $ DoubleValue    $ unsafeFromData v
                        "Bool"           -> return $ Just $ BoolValue      $ unsafeFromData v
                        "Histogram"      -> return $ Just $ Histogram      $ unsafeFromData v
                        "IntPairList"    -> return $ Just $ IntPairList    $ unsafeFromData v
                        "DoublePairList" -> return $ Just $ DoublePairList $ unsafeFromData v
                        {-"Graphics"       -> return $ Just $ Graphics       $ fromGraphics     v-}
                        {-"Layer"          -> return $ Just $ Graphics       $ fromLayer        v-}
                        {-"Geometry"       -> return $ Just $ Graphics       $ fromGeometry     v-}
                        {-"GeoComponent"   -> return $ Just $ Graphics       $ fromGeoComponent v-}
                        {-"Surface"        -> return $ Just $ Graphics       $ fromSurface      v-}
                        {-"Shape"          -> return $ Just $ Graphics       $ fromShape        v-}
                        {-"Primitive"      -> return $ Just $ Graphics       $ fromPrimitive    v-}
                        {-"Figure"         -> return $ Just $ Graphics       $ fromFigure       v-}
                        {-"Material"       -> return $ Just $ Graphics       $ fromMaterial     v-}
                        "List"           -> do
                            args <- ASTBuilder.unpackArguments as
                            case args of
                                [a] -> do
                                    arg <- Builder.read a
                                    caseTest (uncover arg) $ do
                                        of' $ \(Cons (Lit.String n) _) -> case n of
                                            "Int"    -> return $ Just $ IntList    $ unsafeFromData v
                                            "Double" -> return $ Just $ DoubleList $ unsafeFromData v
                                            "Bool"   -> return $ Just $ BoolList   $ unsafeFromData v
                                            "String" -> return $ Just $ StringList $ unsafeFromData v
                                            _        -> return Nothing
                                        of' $ \ANY -> return Nothing
                                _ -> return Nothing
                        _ -> return Nothing
                    of' $ \ANY -> return Nothing

readMeta :: NodeRef -> Command AST (Maybe NodeMeta)
readMeta ref = runASTOp $ HMap.lookup metaKey . view (prop Meta) <$> Builder.read ref

getError :: NodeRef -> Command AST (Maybe (APIError.Error TypeRep))
getError = runASTOp . getError'

getError' :: ASTOp m => NodeRef -> m (Maybe (APIError.Error TypeRep))
getError' ref = do
    n :: ASTNode <- Builder.read ref
    err <- mapM reprError $ n ^. prop TCData . tcErrors
    inps <- mapM (Builder.follow source) $ uncover n # Inputs
    inpErrs <- concat . fmap maybeToList <$> mapM getError' inps
    return $ tryHead err <|> tryHead inpErrs

reprError :: ASTOp m => TCError NodeRef -> m (APIError.Error TypeRep)
reprError tcErr = case tcErr of
    TCError.ImportError Nothing m  -> return $ APIError.ImportError m
    TCError.ImportError (Just n) m -> do
        tp <- Builder.follow source =<< Builder.follow (prop Type) n
        tpRep <- Printer.getTypeRep tp
        return $ APIError.NoMethodError m tpRep
    TCError.UnificationError uni -> do
        uniNode <- Builder.read uni
        caseTest (uncover uniNode) $ do
            of' $ \(Unify l r) -> APIError.TypeError <$> (Printer.getTypeRep =<< Builder.follow source l) <*> (Printer.getTypeRep =<< Builder.follow source r)
            of' $ \ANY         -> impossible

writeMeta :: NodeRef -> NodeMeta -> Command AST ()
writeMeta ref newMeta = runASTOp $ Builder.withRef ref $ prop Meta %~ HMap.insert metaKey newMeta

renameVar :: NodeRef -> String -> Command AST ()
renameVar = runASTOp .: ASTBuilder.renameVar

removeSubtree :: NodeRef -> Command AST ()
removeSubtree = runASTOp . safeRemove

printExpression :: NodeRef -> Command AST String
printExpression = runASTOp . Printer.printExpression

applyFunction :: NodeRef -> NodeRef -> Int -> Command AST NodeRef
applyFunction = runASTOp .:. ASTBuilder.applyFunction

unapplyArgument :: NodeRef -> Int -> Command AST NodeRef
unapplyArgument = runASTOp .: ASTBuilder.removeArg

makeAccessor :: NodeRef -> NodeRef -> Command AST NodeRef
makeAccessor = runASTOp .: ASTBuilder.makeAccessor

removeAccessor :: NodeRef -> Command AST NodeRef
removeAccessor = runASTOp . ASTBuilder.unAcc

getTargetNode :: NodeRef -> Command AST NodeRef
getTargetNode nodeRef = runASTOp $ Builder.read nodeRef
                               >>= return . view ASTBuilder.rightMatchOperand
                               >>= Builder.follow source

getVarNode :: NodeRef -> Command AST NodeRef
getVarNode nodeRef = runASTOp $ Builder.read nodeRef
                            >>= return . view ASTBuilder.leftMatchOperand
                            >>= Builder.follow source

replaceTargetNode :: NodeRef -> NodeRef -> Command AST ()
replaceTargetNode matchNodeId newTargetId = runASTOp $ do
    Builder.reconnect ASTBuilder.rightMatchOperand matchNodeId newTargetId
    return ()

dumpGraphViz :: String -> Command AST ()
dumpGraphViz name = do
    g <- runASTOp Builder.get
    liftIO $ renderAndOpen [(name, name, g)]
