{-# LANGUAGE FlexibleContexts #-}

module Empire.ASTOps.Builder where

import           Prologue
import           Control.Monad.Error (throwError)
import           Data.Record       (match, caseTest, autoCons, ANY(..))
{-import           Data.Layer.Coat     (uncoat, coated)-}

import           Empire.ASTOp           (ASTOp)
import           Empire.ASTOps.Remove   (removeNode, safeRemove)
import           Empire.Data.AST        (ASTNode, ASTEdge, NodeRef, EdgeRef)

import qualified Luna.Syntax.Model.Graph as Graph
import           Luna.Syntax.AST.Term   (Var(..), App(..), Blank(..), Acc(..), Unify(..), Val, Draft)
import qualified Luna.Syntax.AST.Term   as Term
import qualified Luna.Syntax.AST.Arg    as Arg
import           Luna.Syntax.AST.Arg    (Arg)

functionApplicationNode :: Lens' ASTNode (EdgeRef)
functionApplicationNode = coated . lens getter setter where
    getter a   = caseTest a $ match $ \(App f _   ) -> f
    setter a f = caseTest a $ match $ \(App _ args) -> autoCons $ App f args

unpackArguments :: [Arg (EdgeRef)] -> ASTOp [NodeRef]
unpackArguments args = mapM (Graph.follow . Arg.__arec) $ Term.inputs args

isBlank :: NodeRef -> ASTOp Bool
isBlank ref = do
    node <- Graph.read ref
    caseTest (uncoat node) $ do
        match $ \Blank -> return True
        match $ \ANY   -> return False

removeArg :: NodeRef -> Int -> ASTOp (NodeRef)
removeArg fun pos = do
    (f, args)  <- destructApp fun
    freshBlank <- Graph._blank
    let newArgs = args & ix pos .~ freshBlank
    allBlanks <- and <$> mapM isBlank newArgs
    Graph.app f (Graph.arg <$> newArgs)

destructApp :: NodeRef -> ASTOp (NodeRef, [NodeRef])
destructApp fun = do
    app    <- Graph.read fun
    result <- caseTest (uncoat app) $ do
        match $ \(App tg args) -> do
            unpackedArgs <- unpackArguments args
            target <- Graph.follow tg
            return (target, unpackedArgs)
        match $ \ANY -> throwError "Expected App node, got wrong type."
    removeNode fun
    return result

newApplication :: NodeRef -> NodeRef -> Int -> ASTOp (NodeRef)
newApplication fun arg pos = do
    blanks <- sequence $ replicate pos Graph._blank
    let args = blanks ++ [arg]
    Graph.app fun (Graph.arg <$> args)

rewireApplication :: NodeRef -> NodeRef -> Int -> ASTOp (NodeRef)
rewireApplication fun arg pos = do
    (target, oldArgs) <- destructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
        argsCmd    = take argsLength $ (return <$> oldArgs) ++ (repeat Graph._blank)
        oldArgCmd  = argsCmd !! pos
        withNewArg = argsCmd & ix pos .~ return arg

    args <- sequence withNewArg
    oldArg <- oldArgCmd

    whenM (isBlank oldArg) $ removeNode oldArg
    Graph.app target (Graph.arg <$> args)

applyFunction :: NodeRef -> NodeRef -> Int -> ASTOp (NodeRef)
applyFunction fun arg pos = do
    funNode <- Graph.read fun
    caseTest (uncoat funNode) $ do
        match $ \(App _ _) -> rewireApplication fun arg pos
        match $ \ANY -> newApplication fun arg pos

reapply :: NodeRef -> [NodeRef] -> ASTOp (NodeRef)
reapply funRef args = do
    funNode <- Graph.read funRef
    fun <- caseTest (uncoat funNode) $ do
        match $ \(App t _) -> do
            f <- Graph.follow t
            removeNode funRef
            return f
        match $ \ANY -> return funRef
    Graph.app fun $ Graph.arg <$> args

makeAccessorRec :: NodeRef -> NodeRef -> Bool -> ASTOp (NodeRef)
makeAccessorRec targetNodeRef namingNodeRef seenApp = do
    namingNode <- Graph.read namingNodeRef
    caseTest (uncoat namingNode) $ do
        match $ \(Var n) -> do
            stringNodeRef <- Graph.follow n
            removeNode namingNodeRef
            accNode <- Graph.accessor stringNodeRef targetNodeRef
            if seenApp
                then return accNode
                else Graph.app accNode ([] :: [Arg (NodeRef)])
        match $ \(App t _) -> do
            newNamingNodeRef <- Graph.follow t
            replacementRef <- makeAccessorRec targetNodeRef newNamingNodeRef True
            Graph.reconnect namingNodeRef functionApplicationNode replacementRef
            return namingNodeRef
        match $ \(Accessor n t) -> do
            oldTargetRef <- Graph.follow t
            nameRef <- Graph.follow n
            removeNode namingNodeRef
            safeRemove oldTargetRef
            Graph.accessor nameRef targetNodeRef
        match $ \ANY -> throwError "Invalid node type"

makeAccessor :: NodeRef -> NodeRef -> ASTOp (NodeRef)
makeAccessor targetNodeRef namingNodeRef = makeAccessorRec targetNodeRef namingNodeRef False

unAcc :: NodeRef -> ASTOp (NodeRef)
unAcc ref = do
    node <- Graph.read ref
    caseTest (uncoat node) $ do
        match $ \(Accessor n t) -> do
            nameNode <- Graph.follow n
            freshBlank <- Graph._blank
            removeNode ref
            Graph.accessor nameNode freshBlank
        match $ \(App t args) -> do
            target <- Graph.follow t
            replacementRef <- unAcc target
            case args of
                [] -> removeNode ref >> return replacementRef
                as -> Graph.reconnect ref functionApplicationNode replacementRef >> return ref
        match $ \ANY -> throwError "Self port not connected"

unifyWithName :: String -> NodeRef -> ASTOp (NodeRef)
unifyWithName name node = do
    nameVar <- Graph.var name
    Graph.unify nameVar node

withUnifyNode :: NodeRef -> (Unify (EdgeRef) -> ASTOp a) -> ASTOp a
withUnifyNode nodeRef op = do
    node <- Graph.read nodeRef
    caseTest (uncoat node) $ do
        match $ \x   -> op (x :: Unify (EdgeRef))
        match $ \ANY -> throwError "Not a unify node."

rightUnifyOperand :: Lens' ASTNode (EdgeRef)
rightUnifyOperand = coated . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ match $ \(Unify _ r) -> r
    rightSetter u r = caseTest u $ match $ \(Unify l _) -> autoCons $ Unify l r

varName :: Lens' ASTNode (EdgeRef)
varName = coated . lens nameGetter nameSetter where
    nameGetter v   = caseTest v $ match $ \(Var n) -> n
    nameSetter    :: Draft (EdgeRef) -> Ref Edge -> Draft (Ref Edge)
    nameSetter v n = caseTest v $ match $ \(Var _) -> (autoCons $ Var n)

renameVar :: NodeRef -> String -> ASTOp ()
renameVar vref name = do
    node    <- Graph.read vref
    oldName <- Graph.follow $ node ^. varName
    Graph._string name >>= Graph.reconnect vref varName
    safeRemove oldName
