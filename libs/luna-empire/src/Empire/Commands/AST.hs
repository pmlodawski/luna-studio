{-# LANGUAGE FlexibleContexts #-}

module Empire.Commands.AST where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error (throwError)
import           Data.Variants       (match, case', specificCons, ANY(..))
import           Data.Layer.Coat     (uncoat, coated)

import           Empire.Empire
import           Empire.Data.AST              (AST, ASTNode)
import           Empire.Data.WithMeta         (meta)
import           Empire.API.Data.NodeMeta     (NodeMeta)
import           Empire.API.Data.DefaultValue (PortDefault)

import           Empire.ASTOp          (runASTOp)
import qualified Empire.ASTOps.Parse   as Parser
import qualified Empire.ASTOps.Print   as Printer
import qualified Empire.ASTOps.Builder as ASTBuilder
import           Empire.ASTOps.Remove  (safeRemove)

import qualified Luna.Syntax.Builder       as Builder
import           Luna.Syntax.Repr.Graph    (Ref(..), Node(..))
import           Luna.Syntax.AST.Term      (Unify(..))
import           Luna.Syntax.Layer.Labeled (HasLabel, label)

addNode :: String -> String -> Command AST (Ref Node)
addNode name expr = runASTOp $ Parser.parseFragment expr >>= ASTBuilder.unifyWithName name

addDefault :: PortDefault -> Command AST (Ref Node)
addDefault val = runASTOp $ Parser.parsePortDefault val

readMeta :: Ref Node -> Command AST (Maybe NodeMeta)
readMeta ref = runASTOp $ view meta <$> Builder.readRef ref

writeMeta :: Ref Node -> Maybe NodeMeta -> Command AST ()
writeMeta ref newMeta = runASTOp $ do
    node <- Builder.readRef ref
    Builder.writeRef ref (node & meta .~ newMeta)

removeSubtree :: Ref Node -> Command AST ()
removeSubtree = runASTOp . safeRemove

printExpression :: Ref Node -> Command AST String
printExpression = runASTOp . Printer.printExpression

applyFunction :: Ref Node -> Ref Node -> Int -> Command AST (Ref Node)
applyFunction = runASTOp .:. ASTBuilder.applyFunction

unapplyArgument :: Ref Node -> Int -> Command AST (Ref Node)
unapplyArgument = runASTOp .: ASTBuilder.removeArg

makeAccessor :: Ref Node -> Ref Node -> Command AST (Ref Node)
makeAccessor = runASTOp .: ASTBuilder.makeAccessor

removeAccessor :: Ref Node -> Command AST (Ref Node)
removeAccessor = runASTOp . ASTBuilder.unAcc

getTargetNode :: Ref Node -> Command AST (Ref Node)
getTargetNode nodeRef = runASTOp $ ASTBuilder.withUnifyNode nodeRef $ \(Unify _ r) -> Builder.follow r

getVarNode :: Ref Node -> Command AST (Ref Node)
getVarNode nodeRef = runASTOp $ ASTBuilder.withUnifyNode nodeRef $ \(Unify l _) -> Builder.follow l

replaceTargetNode :: Ref Node -> Ref Node -> Command AST ()
replaceTargetNode unifyNodeId newTargetId = runASTOp $ do
    Builder.reconnect unifyNodeId ASTBuilder.rightUnifyOperand newTargetId
    return ()
