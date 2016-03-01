module Empire.ASTOps.Print where

import           Prologue
import           Data.Record            (ANY (..), caseTest, match)
import qualified Data.Text.Lazy         as Text
import           Data.Layer.Cover       (uncover)
import           Data.Direction         (source)

import           Empire.ASTOp           (ASTOp)
import           Empire.Data.AST        (NodeRef)
import qualified Empire.ASTOps.Builder  as ASTBuilder

import           Luna.Syntax.AST.Term   (Acc (..), App (..), Blank (..), Unify (..), Var (..))
import qualified Luna.Syntax.AST.Lit    as Lit
import qualified Luna.Syntax.Builder    as Builder

printExpression' :: ASTOp m => Bool -> NodeRef -> m String
printExpression' suppresNodes nodeRef = do
    let recur = printExpression' suppresNodes
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        match $ \(Unify l r) -> do
            leftRep  <- Builder.follow source l >>= recur
            rightRep <- Builder.follow source r >>= recur
            return $ leftRep ++ " = " ++ rightRep
        match $ \(Var n) -> do
            isNode <- ASTBuilder.isGraphNode nodeRef
            return $ if isNode && suppresNodes then "_" else unwrap n
        match $ \(Acc n t) -> do
            targetRep <- Builder.follow source t >>= recur
            return $ targetRep ++ "." ++ unwrap n
        match $ \(App f args) -> do
            funRep <- Builder.follow source f >>= recur
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- sequence $ recur <$> unpackedArgs
            case argsRep of
                [] -> return funRep
                _  -> return $ funRep ++ " " ++ (intercalate " " argsRep)
        match $ \Blank -> return "_"
        match $ \(Lit.Number _ s) -> return $ case s of
            Lit.Rational r -> show r
            Lit.Integer  i -> show i
        match $ \(Lit.String s) -> return $ show s
        match $ \ANY -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' False

printNodeExpression :: ASTOp m => NodeRef -> m String
printNodeExpression = printExpression' True
