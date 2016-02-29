{-# LANGUAGE ScopedTypeVariables #-}

module Empire.ASTOps.Builder where

import           Prologue                hiding ((#), cons)
import           Control.Monad.Error     (throwError)
import           Data.Record             (match, caseTest, cons, ANY(..))
import           Data.Prop               ((#))
import           Data.Layer.Cover        (uncover, covered)
import           Data.Graph              (Inputs (..))
import           Data.Direction          (source)

import           Empire.ASTOp            (ASTOp)
import           Empire.Empire           ((<?!>))
import           Empire.ASTOps.Remove    (removeNode, safeRemove)
import           Empire.Data.AST         (ASTEdge, ASTNode, EdgeRef, NodeRef, UncoveredNode)
import           Luna.Syntax.AST.Arg     (Arg)
import qualified Luna.Syntax.AST.Arg     as Arg
import           Luna.Syntax.AST.Term    (Acc (..), App (..), Blank (..), Unify (..), Var (..))
import qualified Luna.Syntax.AST.Lit     as Lit
import qualified Luna.Syntax.Builder     as Builder

functionApplicationNode :: Lens' ASTNode EdgeRef
functionApplicationNode = covered . lens getter setter where
    getter a   = caseTest a $ match $ \(App f _   ) -> f
    setter a f = caseTest a $ match $ \(App _ args) -> cons $ App f args

accessorTarget :: Lens' ASTNode EdgeRef
accessorTarget = covered . lens getter setter where
    getter (a :: UncoveredNode)   = caseTest a $ match $ \(Acc _ t) -> t :: EdgeRef
    setter (a :: UncoveredNode) t = caseTest a $ match $ \(Acc n _) -> cons $ Acc n t :: UncoveredNode

unpackArguments :: ASTOp m => [Arg (EdgeRef)] -> m [NodeRef]
unpackArguments args = mapM (Builder.follow source . Arg.__arec) args

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
            target <- Builder.follow source tg
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
            f <- Builder.follow source t
            removeNode funRef
            return f
        match $ \ANY -> return funRef
    Builder.app fun $ Builder.arg <$> args

makeAccessorRec :: ASTOp m => Bool -> NodeRef -> NodeRef -> m NodeRef
makeAccessorRec seenApp targetNodeRef namingNodeRef = do
    namingNode <- Builder.read namingNodeRef
    caseTest (uncover namingNode) $ do
        match $ \(Var name) -> do
            removeNode namingNodeRef
            Builder.acc name targetNodeRef >>= (if seenApp then return else flip Builder.app [])
        match $ \(App t _) -> do
            newNamingNodeRef <- Builder.follow source t
            replacementRef <- makeAccessorRec True targetNodeRef newNamingNodeRef
            Builder.reconnect functionApplicationNode namingNodeRef replacementRef
            return namingNodeRef
        match $ \(Acc name t) -> do
            newNamingNodeRef <- Builder.follow source t
            replacement <- makeAccessorRec False targetNodeRef newNamingNodeRef
            Builder.reconnect accessorTarget namingNodeRef replacement
            return namingNodeRef
        match $ \ANY -> throwError "Invalid node type"

makeAccessor :: ASTOp m => NodeRef -> NodeRef -> m NodeRef
makeAccessor = makeAccessorRec False

unAccRec :: forall m. ASTOp m => NodeRef -> m (Maybe NodeRef)
unAccRec ref = do
    node <- Builder.read ref
    caseTest (uncover node) $ do
        match $ \(Acc n t) -> do
            oldTarget <- Builder.follow source t
            replacement <- unAccRec =<< Builder.follow source t
            case replacement of
                Just r  -> do
                    Builder.reconnect accessorTarget ref r
                    return $ Just ref
                Nothing -> removeNode ref >> Just <$> Builder.var n
        match $ \(App t args) -> do
            oldTarget   <- Builder.follow source t
            replacement <- unAccRec oldTarget
            case replacement of
                Just r -> do
                    repl <- Builder.read r
                    caseTest (uncover repl) $ do
                        match $ \(Var _) -> if null args
                            then removeNode ref >> return (Just r)
                            else Builder.reconnect functionApplicationNode ref r >> return (Just ref)
                        match $ \ANY -> do
                            Builder.reconnect functionApplicationNode ref r
                            return $ Just ref
                Nothing -> throwError "Self port not connected"
        match $ \(Unify _ _) -> return Nothing
        match $ \ANY -> removeNode ref >> return Nothing

unAcc :: ASTOp m => NodeRef -> m NodeRef
unAcc ref = unAccRec ref <?!> "Self port not connected"

unifyWithName :: forall m. ASTOp m => String -> NodeRef -> m NodeRef
unifyWithName name node = do
    nameVar <- Builder.var (fromString name) :: m NodeRef
    Builder.unify nameVar node

rightUnifyOperand :: Lens' ASTNode (EdgeRef)
rightUnifyOperand = covered . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ match $ \(Unify _ r) -> r
    rightSetter u r = caseTest u $ match $ \(Unify l _) -> cons $ Unify l r

leftUnifyOperand :: Lens' ASTNode (EdgeRef)
leftUnifyOperand = covered . lens rightGetter rightSetter where
    rightGetter u   = caseTest u $ match $ \(Unify l _) -> l
    rightSetter u l = caseTest u $ match $ \(Unify _ r) -> cons $ Unify l r

varName :: Lens' ASTNode String
varName = covered . lens nameGetter nameSetter where
    nameGetter v   = caseTest v $ match $ \(Var (n :: Lit.String)) -> toString n
    nameSetter v n = caseTest v $ match $ \(Var (_ :: Lit.String)) -> (cons $ Var $ (fromString n :: Lit.String))

renameVar :: ASTOp m => NodeRef -> String -> m ()
renameVar vref name = do
    node    <- Builder.read vref
    Builder.write vref $ node & varName .~ name
