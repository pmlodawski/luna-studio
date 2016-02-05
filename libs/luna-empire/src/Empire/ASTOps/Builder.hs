{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Builder where

import           Prologue            hiding ((#), cons)
import           Control.Monad.Error (throwError)
import           Data.Record       (match, caseTest, cons, ANY(..), Match, MatchSet)
import           Data.Prop         ((#))
import           Data.Layer.Cover    (uncover, covered)

import           Empire.ASTOp            (ASTOp)
import           Empire.ASTOps.Remove    (removeNode, safeRemove)
import           Empire.Data.AST         (ASTEdge, ASTNode, EdgeRef, NodeRef)

import           Luna.Syntax.AST.Arg     (Arg)
import qualified Luna.Syntax.AST.Arg     as Arg
import           Luna.Syntax.AST.Term    (Acc (..), App (..), Blank (..), Unify (..), Var (..), Str (..))
import qualified Luna.Syntax.AST.Term   as Term

import qualified Luna.Syntax.Builder    as Builder
import           Luna.Syntax.Builder (Inputs, target)

functionApplicationNode :: Lens' ASTNode EdgeRef
functionApplicationNode = covered . lens getter setter where
    getter a   = caseTest a $ match $ \(App f _   ) -> f
    setter a f = caseTest a $ match $ \(App _ args) -> cons $ App f args

unpackArguments :: ASTOp m => [Arg (EdgeRef)] -> m [NodeRef]
unpackArguments args = mapM (Builder.follow target . Arg.__arec) args

isBlank :: ASTOp m => NodeRef -> m Bool
isBlank ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        match $ \Blank -> return True
        match $ \ANY   -> return False

removeArg :: ASTOp m => NodeRef -> Int -> m NodeRef
removeArg fun pos = do
    (f, args)  <- destructApp fun
    freshBlank <- Builder.blank
    let newArgs = args & ix pos .~ freshBlank
    allBlanks <- and <$> mapM isBlank newArgs
    Builder.app f (Builder.arg <$> newArgs)

destructApp :: ASTOp m => NodeRef -> m (NodeRef, [NodeRef])
destructApp fun = do
    app    <- Builder.read fun
    result <- caseTest (uncover app) $ do
        match $ \(App tg args) -> do
            unpackedArgs <- unpackArguments args
            target <- Builder.follow target tg
            return (target, unpackedArgs)
        match $ \ANY -> throwError "Expected App node, got wrong type."
    removeNode fun
    return result

newApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
newApplication fun arg pos = do
    blanks <- sequence $ replicate pos Builder.blank
    let args = blanks ++ [arg]
    Builder.app fun (Builder.arg <$> args)

rewireApplication :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
rewireApplication fun arg pos = do
    (target, oldArgs) <- destructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
        argsCmd    = take argsLength $ (return <$> oldArgs) ++ (repeat Builder.blank)
        oldArgCmd  = argsCmd !! pos
        withNewArg = argsCmd & ix pos .~ return arg

    args <- sequence withNewArg
    oldArg <- oldArgCmd

    whenM (isBlank oldArg) $ removeNode oldArg
    Builder.app target (Builder.arg <$> args)

applyFunction :: ASTOp m => NodeRef -> NodeRef -> Int -> m NodeRef
applyFunction fun arg pos = do
    funNode <- Builder.read fun
    caseTest (uncover funNode) $ do
        match $ \(App _ _) -> rewireApplication fun arg pos
        match $ \ANY -> newApplication fun arg pos

reapply :: ASTOp m => NodeRef -> [NodeRef] -> m NodeRef
reapply funRef args = do
    funNode <- Builder.read funRef
    fun <- caseTest (uncover funNode) $ do
        match $ \(App t _) -> do
            f <- Builder.follow target t
            removeNode funRef
            return f
        match $ \ANY -> return funRef
    Builder.app fun $ Builder.arg <$> args

makeAccRec :: ASTOp m => NodeRef -> NodeRef -> Bool -> m NodeRef
makeAccRec targetNodeRef namingNodeRef seenApp = do
    namingNode <- Builder.read namingNodeRef
    caseTest (uncover namingNode) $ do
        match $ \(Var name) -> do
            accNode <- Builder.acc name targetNodeRef
            if seenApp
                then return accNode
                else Builder.app accNode ([] :: [Arg NodeRef])
        match $ \(App t _) -> do
            newNamingNodeRef <- Builder.follow target t
            replacementRef <- makeAccRec targetNodeRef newNamingNodeRef True
            Builder.reconnect namingNodeRef functionApplicationNode replacementRef
            return namingNodeRef
        match $ \(Acc name t) -> do
            oldTargetRef <- Builder.follow target t
            safeRemove oldTargetRef
            Builder.acc name targetNodeRef
        match $ \ANY -> throwError "Invalid node type"

makeAcc :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
makeAcc targetNodeRef namingNodeRef = makeAccRec targetNodeRef namingNodeRef False

unAcc :: forall m. ASTOp m => NodeRef -> m NodeRef
unAcc ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        match $ \(Acc n t) -> do
            freshBlank <- Builder.blank :: m NodeRef
            removeNode ref
            Builder.acc n freshBlank
        match $ \(App t args) -> do
            target <- Builder.follow target t
            replacementRef <- unAcc target
            case args of
                [] -> removeNode ref >> return replacementRef
                as -> Builder.reconnect ref functionApplicationNode replacementRef >> return ref
        match $ \ANY -> throwError "Self port not connected"

unifyWithName :: forall m. ASTOp m => String -> NodeRef -> m NodeRef
unifyWithName name node = do
    nameVar <- Builder.var (Str name) :: m NodeRef
    Builder.unify nameVar node

withUnifyNode :: ASTOp m => NodeRef -> (Unify (EdgeRef) -> m a) -> m a
withUnifyNode nodeRef op = do
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \x   -> op (x :: Unify (EdgeRef))
        match $ \ANY -> throwError "Not a unify node."

rightUnifyOperand :: Lens' ASTNode (EdgeRef)
rightUnifyOperand = covered . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ match $ \(Unify _ r) -> r
    rightSetter u r = caseTest u $ match $ \(Unify l _) -> cons $ Unify l r

varName :: Lens' ASTNode String
varName = covered . lens nameGetter nameSetter where
    nameGetter v   = caseTest v $ match $ \(Var (Str n)) -> n
    nameSetter v n = caseTest v $ match $ \(Var (_ :: Str)) -> (cons $ Var (Str n))

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = do
    node    <- Builder.read vref
    Builder.write vref $ node & varName .~ name
