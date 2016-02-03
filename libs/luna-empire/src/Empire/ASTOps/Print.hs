module Empire.ASTOps.Print where

import           Prologue
import           Data.Record          (match, case', ANY(..))
{-import           Data.Layer.Coat        (uncoat)-}
import qualified Data.Text.Lazy         as Text
import           Empire.ASTOp           (ASTOp)
import qualified Empire.ASTOps.Builder  as ASTBuilder

import qualified Luna.Syntax.Model.Graph.Term as Builder
import           Luna.Syntax.Model.Graph (Ref)
import           Luna.Syntax.AST.Term   (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Val)

printIdent :: Ref Node -> ASTOp String
printIdent nodeRef = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ match $ \(Lit.String n) -> return (Text.unpack . toText $ n)

printVal :: Val a -> String
printVal val = case' val $ match $ \lit -> case lit of
    Lit.String s -> show s
    Lit.Int    i -> show i

printExpression' :: [Ref Node] -> Ref Node -> ASTOp String
printExpression' shadowVars nodeRef = do
    let recur = printExpression' shadowVars
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ do
        match $ \(Unify l r) -> do
            leftRep  <- Builder.follow l >>= recur
            rightRep <- Builder.follow r >>= recur
            return $ leftRep ++ " = " ++ rightRep
        match $ \(Var n) ->
            if elem nodeRef shadowVars
                then return "_"
                else Builder.follow n >>= printIdent
        match $ \(Accessor n t) -> do
            targetRep <- Builder.follow t >>= recur
            nameRep   <- Builder.follow n >>= printIdent
            return $ targetRep ++ "." ++ nameRep
        match $ \(App f args) -> do
            funRep <- Builder.follow f >>= recur
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- sequence $ recur <$> unpackedArgs
            case argsRep of
                [] -> return funRep
                _  -> return $ funRep ++ " " ++ (intercalate " " argsRep)
        match $ \Blank -> return "_"
        match $ return . printVal
        match $ \ANY -> return ""

printExpression :: Ref Node -> ASTOp String
printExpression = printExpression' []

printNodeExpression :: [Ref Node] -> Ref Node -> ASTOp String
printNodeExpression = printExpression'
