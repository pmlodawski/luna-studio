{-# LANGUAGE FlexibleContexts #-}

module Empire.Commands.AST where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error (ErrorT, runErrorT, throwError)
import           Data.Variants       (match, case', specificCons, ANY(..))
import           Data.Layer.Coat     (uncoat, coated)
import           Data.Maybe          (fromMaybe)
import           Data.Construction   (destruct)

import           Empire.Data.AST (AST, ASTNode)
import           Empire.Empire

import           Luna.Syntax.Builder.Star  (StarBuilderT)
import qualified Luna.Syntax.Builder.Star  as StarBuilder
import           Luna.Syntax.Builder.Node  (NodeBuilderT)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder
import           Luna.Syntax.Builder.Class (BuilderT)
import qualified Luna.Syntax.Builder       as Builder
import           Luna.Syntax.Repr.Graph    (Ref(..), Node(..), Edge(..))
import qualified Luna.Syntax.Repr.Graph    as Graph
import           Luna.Syntax.AST.Term      (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Draft)
import qualified Luna.Syntax.AST.Term      as Term
import qualified Luna.Syntax.AST.Typed     as Typed
import qualified Luna.Syntax.AST.Arg       as Arg

import           Empire.Utils.ParserMock   as Parser

type ASTOp = ErrorT Error (NodeBuilderT (Ref Node) (BuilderT AST (StarBuilderT (Maybe (Ref Node)) IO)))

runAstOp :: ASTOp a -> Command AST a
runAstOp cmd = empire $ \g -> flip StarBuilder.evalT Nothing
             $ flip Builder.runT g
             $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
             $ runErrorT
             $ cmd

addNode :: String -> String -> Command AST (Ref Node)
addNode name expr = runAstOp $ fromMaybe (addExpr name expr) (whenString <|> whenInt) where
    whenString = addString  name <$> Parser.asString  expr
    whenInt    = addInteger name <$> Parser.asInteger expr

addInteger :: String -> Int -> ASTOp (Ref Node)
addInteger name num = Builder._int num >>= unifyWithName name

addExpr :: String -> String -> ASTOp (Ref Node)
addExpr name expr = Builder.var expr >>= unifyWithName name

addString :: String -> String -> ASTOp (Ref Node)
addString name lit = Builder._string lit >>= unifyWithName name

unifyWithName :: String -> Ref Node -> ASTOp (Ref Node)
unifyWithName name node = do
    nameVar <- Builder.var name
    Builder.unify nameVar node

withUnifyNode :: Ref Node -> (Unify (Ref Edge) -> ASTOp a) -> ASTOp a
withUnifyNode nodeRef op = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ do
        match $ \x   -> op (x :: Unify (Ref Edge))
        match $ \ANY -> throwError "Not a unify node."

getTargetNode :: Ref Node -> Command AST (Ref Node)
getTargetNode nodeRef = runAstOp $ withUnifyNode nodeRef $ \(Unify _ r) -> Builder.follow r

getVarNode :: Ref Node -> Command AST (Ref Node)
getVarNode nodeRef = runAstOp $ withUnifyNode nodeRef $ \(Unify l _) -> Builder.follow l

rightUnifyOperand :: Lens' ASTNode (Ref Edge)
rightUnifyOperand = coated . lens rightGetter rightSetter where
    rightGetter u   = case' u $ match $ \(Unify _ r) -> r
    rightSetter u r = case' u $ match $ \(Unify l _) -> specificCons $ Unify l r

replaceTargetNode :: Ref Node -> Ref Node -> Command AST ()
replaceTargetNode unifyNodeId newTargetId = runAstOp $ do
    Builder.reconnect unifyNodeId rightUnifyOperand newTargetId
    return ()

makeVar :: Ref Node -> Command AST (Ref Node)
makeVar = runAstOp . Builder.var

makeAccessor :: Ref Node -> Ref Node -> Command AST (Ref Node)
makeAccessor src dst = runAstOp $ Builder.accessor dst src

removeNode' :: Ref Node -> ASTOp ()
removeNode' ref = do
    node     <- Builder.readRef ref
    typeNode <- Builder.follow $ node ^. Typed.tp
    destruct typeNode
    destruct ref

removeIfBlank :: Ref Node -> ASTOp ()
removeIfBlank ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \Blank -> removeNode' ref
        match $ \ANY -> return ()

removeNode :: Ref Node -> Command AST ()
removeNode = runAstOp . removeNode'

getNameNode :: Ref Node -> Command AST (Ref Node)
getNameNode ref = runAstOp $ do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Var n)        -> Builder.follow n
        match $ \(Accessor n _) -> Builder.follow n
        match $ \ANY            -> throwError "It does not have a name, you dummy!"

removeArg :: Ref Node -> Int -> Command AST (Ref Node)
removeArg fun pos = runAstOp $ do
    (f, args)  <- destructApp fun
    freshBlank <- Builder._blank
    let newArgs = args & ix pos .~ freshBlank
    Builder.app f (Builder.arg <$> newArgs)

destructApp :: Ref Node -> ASTOp (Ref Node, [Ref Node])
destructApp fun = do
    app    <- Builder.readRef fun
    result <- case' (uncoat app) $ do
        match $ \(App tg args) -> do
            unpackedArgs <- mapM (Builder.follow . Arg.__arec) $ Term.inputs args
            target <- Builder.follow tg
            return (target, unpackedArgs)
        match $ \ANY -> throwError "Expected App node, got wrong type."
    removeNode' fun
    return result

newApplication :: Ref Node -> Ref Node -> Int -> ASTOp (Ref Node)
newApplication fun arg pos = do
    blanks <- sequence $ replicate pos Builder._blank
    let args = blanks ++ [arg]
    Builder.app fun (Builder.arg <$> args)

rewireApplication :: Ref Node -> Ref Node -> Int -> ASTOp (Ref Node)
rewireApplication fun arg pos = do
    (target, oldArgs) <- destructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
        argsCmd    = take argsLength $ (return <$> oldArgs) ++ (repeat Builder._blank)
        oldArgCmd  = argsCmd !! pos
        withNewArg = argsCmd & ix pos .~ return arg

    args <- sequence withNewArg
    oldArg <- oldArgCmd

    removeIfBlank oldArg
    Builder.app target (Builder.arg <$> args)

applyFunction :: Ref Node -> Ref Node -> Int -> Command AST (Ref Node)
applyFunction fun arg pos = runAstOp $ do
    funNode <- Builder.readRef fun
    case' (uncoat funNode) $ do
        match $ \(App _ _) -> rewireApplication fun arg pos
        match $ \ANY -> newApplication fun arg pos

removeGraphNode' :: Ref Node -> ASTOp ()
removeGraphNode' nodeId = do
    node <- Builder.readRef nodeId
    {-putStrLn "Removing node with succs: "-}
    {-succs <- mapM Builder.unfollow $ (Ref . Edge) <$> (toList $ node ^. Graph.succs)-}
    {-print succs-}
    case' (uncoat node) $ do
        match $ \(App f args) -> do
            removeNode' nodeId
            Builder.follow f >>= removeGraphNode'
        match $ \(Accessor n tg) -> do
            Builder.follow n >>= removeNode'
            removeNode' nodeId
        match $ \(Var n) -> do
            Builder.follow n >>= removeNode'
            removeNode' nodeId
        match $ \ANY -> throwError "Can't remove, screw you xD"

removeGraphNode :: Ref Node -> Command AST ()
removeGraphNode = runAstOp . removeGraphNode'
