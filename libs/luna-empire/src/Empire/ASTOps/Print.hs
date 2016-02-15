module Empire.ASTOps.Print where

import           Prologue
import           Data.Record           (ANY (..), caseTest, match)
import qualified Data.Text.Lazy        as Text
import           Data.Layer.Cover      (uncover)
import           Data.Direction        (source)

import           Empire.ASTOp          (ASTOp)
import           Empire.Data.AST       (NodeRef)
import qualified Empire.ASTOps.Builder as ASTBuilder

import           Luna.Syntax.AST.Term  (Acc (..), App (..), Blank (..), Unify (..), Var (..), Num (..), Str (..))
import qualified Luna.Syntax.Builder   as Builder

printExpression' :: ASTOp m => [NodeRef] -> NodeRef -> m String
printExpression' shadowVars nodeRef = do
    let recur = printExpression' shadowVars
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \(Unify l r) -> do
            leftRep  <- Builder.follow source l >>= recur
            rightRep <- Builder.follow source r >>= recur
            return $ leftRep ++ " = " ++ rightRep
        match $ \(Var n) ->
            if elem nodeRef shadowVars
                then return "_"
                else return $ toString n
        match $ \(Acc n t) -> do
            targetRep <- Builder.follow source t >>= recur
            return $ targetRep ++ "." ++ toString n
        match $ \(App f args) -> do
            funRep <- Builder.follow source f >>= recur
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- sequence $ recur <$> unpackedArgs
            case argsRep of
                [] -> return funRep
                _  -> return $ funRep ++ " " ++ (intercalate " " argsRep)
        match $ \Blank -> return "_"
        match $ \(Num i) -> return $ show i
        match $ \(Str s) -> return $ show s
        match $ \ANY -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' []

printNodeExpression :: ASTOp m => [NodeRef] -> NodeRef -> m String
printNodeExpression = printExpression'
