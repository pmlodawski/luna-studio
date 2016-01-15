module Empire.ASTOps.Print where

import           Prologue
import           Data.Variants          (match, case', ANY(..))
import           Data.Layer.Coat        (uncoat)
import qualified Data.Text.Lazy         as Text
import           Empire.ASTOp           (ASTOp)
import qualified Empire.ASTOps.Builder  as ASTBuilder

import qualified Luna.Syntax.Builder    as Builder
import           Luna.Syntax.Repr.Graph (Ref, Node)
import           Luna.Syntax.AST.Term   (Var(..), App(..), Blank(..), Accessor(..), Unify(..), Val)
import qualified Luna.Syntax.AST.Lit    as Lit

printIdent :: Ref Node -> ASTOp String
printIdent nodeRef = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ match $ \(Lit.String n) -> return (Text.unpack . toText $ n)

printVal :: Val a -> String
printVal val = case' val $ match $ \lit -> case lit of
    Lit.String s -> show s
    Lit.Int    i -> show i

printExpression :: Ref Node -> ASTOp String
printExpression nodeRef = do
    node <- Builder.readRef nodeRef
    case' (uncoat node) $ do
        match $ \(Unify l r) -> do
            leftRep  <- Builder.follow l >>= printExpression
            rightRep <- Builder.follow r >>= printExpression
            return $ leftRep ++ " = " ++ rightRep
        match $ \(Var n) -> Builder.follow n >>= printIdent
        match $ \(Accessor n t) -> do
            targetRep <- Builder.follow t >>= printExpression
            nameRep   <- Builder.follow n >>= printIdent
            return $ targetRep ++ "." ++ nameRep
        match $ \(App f args) -> do
            funRep <- Builder.follow f >>= printExpression
            unpackedArgs <- ASTBuilder.unpackArguments args
            argsRep <- sequence $ printExpression <$> unpackedArgs
            case argsRep of
                [] -> return funRep
                _  -> return $ funRep ++ " " ++ (intercalate " " argsRep)
        match $ \Blank -> return "_"
        match $ return . printVal
        match $ \ANY -> return ""
