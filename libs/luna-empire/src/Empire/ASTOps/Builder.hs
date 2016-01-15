{-# LANGUAGE FlexibleContexts #-}

module Empire.ASTOps.Builder where

import           Prologue
import           Control.Monad.Error (throwError)
import           Data.Variants       (match, case', specificCons, ANY(..))
import           Data.Layer.Coat     (uncoat, coated)

import           Empire.ASTOp           (ASTOp)
import           Empire.ASTOps.Remove   (removeNode, safeRemove)
import           Empire.Data.AST        (ASTNode)

import qualified Luna.Syntax.Builder    as Builder
import           Luna.Syntax.Repr.Graph (Ref(..), Node(..), Edge(..))
import           Luna.Syntax.AST.Term   (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Val)
import qualified Luna.Syntax.AST.Term   as Term
import qualified Luna.Syntax.AST.Arg    as Arg
import           Luna.Syntax.AST.Arg    (Arg)

functionApplicationNode :: Lens' ASTNode (Ref Edge)
functionApplicationNode = coated . lens getter setter where
    getter a   = case' a $ match $ \(App f _   ) -> f
    setter a f = case' a $ match $ \(App _ args) -> specificCons $ App f args

unpackArguments :: [Arg (Ref Edge)] -> ASTOp [Ref Node]
unpackArguments args = mapM (Builder.follow . Arg.__arec) $ Term.inputs args

isBlank :: Ref Node -> ASTOp Bool
isBlank ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \Blank -> return True
        match $ \ANY   -> return False

removeArg :: Ref Node -> Int -> ASTOp (Ref Node)
removeArg fun pos = do
    (f, args)  <- destructApp fun
    freshBlank <- Builder._blank
    let newArgs = args & ix pos .~ freshBlank
    allBlanks <- and <$> mapM isBlank newArgs

    if allBlanks
        then mapM removeNode newArgs >> return f
        else Builder.app f (Builder.arg <$> newArgs)

destructApp :: Ref Node -> ASTOp (Ref Node, [Ref Node])
destructApp fun = do
    app    <- Builder.readRef fun
    result <- case' (uncoat app) $ do
        match $ \(App tg args) -> do
            unpackedArgs <- unpackArguments args
            target <- Builder.follow tg
            return (target, unpackedArgs)
        match $ \ANY -> throwError "Expected App node, got wrong type."
    removeNode fun
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

    whenM (isBlank oldArg) $ removeNode oldArg
    Builder.app target (Builder.arg <$> args)

applyFunction :: Ref Node -> Ref Node -> Int -> ASTOp (Ref Node)
applyFunction fun arg pos = do
    funNode <- Builder.readRef fun
    case' (uncoat funNode) $ do
        match $ \(App _ _) -> rewireApplication fun arg pos
        match $ \ANY -> newApplication fun arg pos

reapply :: Ref Node -> [Ref Node] -> ASTOp (Ref Node)
reapply funRef args = do
    funNode <- Builder.readRef funRef
    fun <- case' (uncoat funNode) $ do
        match $ \(App t _) -> do
            f <- Builder.follow t
            removeNode funRef
            return f
        match $ \ANY -> return funRef
    Builder.app fun $ Builder.arg <$> args

makeAccessorRec :: Ref Node -> Ref Node -> Bool -> ASTOp (Ref Node)
makeAccessorRec targetNodeRef namingNodeRef seenApp = do
    namingNode <- Builder.readRef namingNodeRef
    case' (uncoat namingNode) $ do
        match $ \(Var n) -> do
            stringNodeRef <- Builder.follow n
            removeNode namingNodeRef
            accNode <- Builder.accessor stringNodeRef targetNodeRef
            if seenApp
                then return accNode
                else Builder.app accNode ([] :: [Arg (Ref Node)])
        match $ \(App t _) -> do
            newNamingNodeRef <- Builder.follow t
            replacementRef <- makeAccessorRec targetNodeRef newNamingNodeRef True
            Builder.reconnect namingNodeRef functionApplicationNode replacementRef
            return namingNodeRef
        match $ \(Accessor n t) -> do
            oldTargetRef <- Builder.follow t
            nameRef <- Builder.follow n
            removeNode namingNodeRef
            safeRemove oldTargetRef
            Builder.accessor nameRef targetNodeRef
        match $ \ANY -> throwError "Invalid node type"

makeAccessor :: Ref Node -> Ref Node -> ASTOp (Ref Node)
makeAccessor targetNodeRef namingNodeRef = makeAccessorRec targetNodeRef namingNodeRef False

unAcc :: Ref Node -> ASTOp (Ref Node)
unAcc ref = do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Accessor n t) -> do
            nameNode <- Builder.follow n
            removeNode ref
            Builder.var nameNode
        match $ \(App t args) -> do
            target <- Builder.follow t
            replacementRef <- unAcc target
            case args of
                [] -> removeNode ref >> return replacementRef
                as -> Builder.reconnect ref functionApplicationNode replacementRef >> return ref
        match $ \ANY -> throwError "Self port not connected"

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

rightUnifyOperand :: Lens' ASTNode (Ref Edge)
rightUnifyOperand = coated . lens rightGetter rightSetter where
    rightGetter u   = case' u $ match $ \(Unify _ r) -> r
    rightSetter u r = case' u $ match $ \(Unify l _) -> specificCons $ Unify l r
