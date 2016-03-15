module Empire.ASTOps.Print where

import           Prologue
import           Data.Record              (ANY (..), caseTest, of')
import qualified Data.Text.Lazy           as Text
import           Data.Layer.Cover         (uncover)
import           Data.Direction           (source)

import           Empire.ASTOp             (ASTOp)
import           Empire.Data.AST          (NodeRef)
import qualified Empire.ASTOps.Builder    as ASTBuilder

import           Luna.Syntax.AST.Term     (Acc (..), App (..), Blank (..), Match (..), Var (..))
import qualified Luna.Syntax.AST.Term.Lit as Lit

import qualified Luna.Syntax.Model.Network.Builder as Builder

printExpression' :: ASTOp m => Bool -> Bool -> NodeRef -> m String
printExpression' suppresNodes paren nodeRef = do
    let recur = printExpression' suppresNodes
    node <- Builder.read nodeRef
    caseTest (uncover node) $ do
        of' $ \(Match l r) -> do
            leftRep  <- Builder.follow source l >>= recur paren
            rightRep <- Builder.follow source r >>= recur paren
            return $ leftRep ++ " = " ++ rightRep
        of' $ \(Var n) -> do
            isNode <- ASTBuilder.isGraphNode nodeRef
            return $ if isNode && suppresNodes then "_" else unwrap n
        of' $ \(Acc n t) -> do
            targetRep <- Builder.follow source t >>= recur True
            if targetRep == "_"
                then return $ unwrap n
                else return $ targetRep ++ "." ++ unwrap n
        of' $ \(App f args) -> do
            funRep <- Builder.follow source f >>= recur True
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- sequence $ recur True <$> unpackedArgs
            let shouldParen = paren && not (null args)
            case argsRep of
                [] -> return funRep
                _  -> return $ (if shouldParen then "(" else "")
                            ++ funRep
                            ++ " "
                            ++ (intercalate " " argsRep)
                            ++ (if shouldParen then ")" else "")
        of' $ \Blank -> return "_"
        of' $ \(Lit.Number _ s) -> return $ case s of
            Lit.Rational r -> show r
            Lit.Integer  i -> show i
            Lit.Double   d -> show d
        of' $ \(Lit.String s) -> return $ show s
        of' $ \ANY -> return ""

printExpression :: ASTOp m => NodeRef -> m String
printExpression = printExpression' False False

printNodeExpression :: ASTOp m => NodeRef -> m String
printNodeExpression = printExpression' True False
