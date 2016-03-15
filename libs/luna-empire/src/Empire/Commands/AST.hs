{-# LANGUAGE FlexibleContexts    #-}

module Empire.Commands.AST where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error          (throwError)
import           Data.Record                  (ANY (..), caseTest, of')
import           Data.Prop                    (prop)
import           Data.Graph                   (source)
import           Data.HMap.Lazy               (TypeKey (..))
import qualified Data.HMap.Lazy               as HMap

import           Empire.Empire
import           Empire.API.Data.DefaultValue (PortDefault, Value (..))
import           Empire.API.Data.NodeMeta     (NodeMeta)
import           Empire.API.Data.Node         (NodeId)
import           Empire.Data.NodeMarker       (NodeMarker (..))
import           Empire.Data.AST              (AST, NodeRef, ASTNode)

import           Empire.ASTOp                 (runASTOp)
import qualified Empire.ASTOps.Builder        as ASTBuilder
import qualified Empire.ASTOps.Parse          as Parser
import qualified Empire.ASTOps.Print          as Printer
import           Empire.ASTOps.Remove         (safeRemove)

import           Luna.Diagnostic.Vis.GraphViz (renderAndOpen)

import           Luna.Syntax.Model.Network.Builder (Meta (..), Type (..))
import qualified Luna.Syntax.Model.Network.Builder as Builder
import           Luna.Syntax.AST.Term              (Cons (..))
import qualified Luna.Syntax.AST.Term.Lit          as Lit

import qualified Luna.Compilation.Pass.Interpreter.Layer as Interpreter
import           Luna.Compilation.Pass.Interpreter.Layer (InterpreterData (..))
import           Unsafe.Coerce

metaKey :: TypeKey NodeMeta
metaKey = TypeKey

addNode :: NodeId -> String -> String -> Command AST (NodeRef)
addNode nid name expr = runASTOp $ Parser.parseExpr expr >>= ASTBuilder.makeNodeRep (NodeMarker nid) name

addDefault :: PortDefault -> Command AST (NodeRef)
addDefault val = runASTOp $ Parser.parsePortDefault val

getNodeValue :: NodeRef -> Command AST (Maybe Value)
getNodeValue ref = runASTOp $ do
    node   <- Builder.read ref
    tp     <- Builder.follow source $ node ^. prop Type
    tpNode <- Builder.read tp
    case (node ^. prop InterpreterData . Interpreter.value) of
        Nothing -> return Nothing
        Just v  -> caseTest (uncover tpNode) $ do
            of' $ \(Cons (Lit.String n) as) -> case n of
                "Int"      -> return $ Just $ IntValue    $ unsafeCoerce v
                "String"   -> return $ Just $ StringValue $ unsafeCoerce v
                "Double"   -> return $ Just $ DoubleValue $ unsafeCoerce v
                "Bool"     -> return $ Just $ BoolValue   $ unsafeCoerce v
                "List"     -> do
                    args <- ASTBuilder.unpackArguments as
                    case args of
                        [a] -> do
                            arg <- Builder.read a
                            caseTest (uncover arg) $ do
                                of' $ \(Cons (Lit.String n) _) -> case n of
                                    "Int"    -> return $ Just $ IntList    $ unsafeCoerce v
                                    "Double" -> return $ Just $ DoubleList $ unsafeCoerce v
                                    _        -> return Nothing
                                of' $ \ANY -> return Nothing
                        _ -> return Nothing
                _ -> return Nothing
            of' $ \ANY -> return Nothing

readMeta :: NodeRef -> Command AST (Maybe NodeMeta)
readMeta ref = runASTOp $ HMap.lookup metaKey . view (prop Meta) <$> Builder.read ref

writeMeta :: NodeRef -> NodeMeta -> Command AST ()
writeMeta ref newMeta = runASTOp $ do
    Builder.withRef ref $ prop Meta %~ HMap.insert metaKey newMeta

renameVar :: NodeRef -> String -> Command AST ()
renameVar = runASTOp .: ASTBuilder.renameVar

removeSubtree :: NodeRef -> Command AST ()
removeSubtree = runASTOp . safeRemove

printExpression :: NodeRef -> Command AST String
printExpression = runASTOp . Printer.printExpression

applyFunction :: NodeRef -> NodeRef -> Int -> Command AST (NodeRef)
applyFunction = runASTOp .:. ASTBuilder.applyFunction

unapplyArgument :: NodeRef -> Int -> Command AST (NodeRef)
unapplyArgument = runASTOp .: ASTBuilder.removeArg

makeAccessor :: NodeRef -> NodeRef -> Command AST (NodeRef)
makeAccessor = runASTOp .: ASTBuilder.makeAccessor

removeAccessor :: NodeRef -> Command AST (NodeRef)
removeAccessor = runASTOp . ASTBuilder.unAcc

getTargetNode :: NodeRef -> Command AST (NodeRef)
getTargetNode nodeRef = runASTOp $ Builder.read nodeRef
                               >>= return . view ASTBuilder.rightMatchOperand
                               >>= Builder.follow source

getVarNode :: NodeRef -> Command AST (NodeRef)
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
