module Empire.Commands.AST where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error (ErrorT, runErrorT, throwError)
import           Data.Variants       (match, case', ANY(..))
import           Data.Layer.Coat     (uncoat)
import           Data.Maybe          (fromMaybe)

import           Empire.Data.AST (AST)
import           Empire.Empire

import           Luna.Syntax.Builder.Star  (StarBuilderT)
import qualified Luna.Syntax.Builder.Star  as StarBuilder
import           Luna.Syntax.Builder.Node  (NodeBuilderT)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder
import           Luna.Syntax.Builder.Class (BuilderT)
import qualified Luna.Syntax.Builder       as Builder
import           Luna.Syntax.Repr.Graph    (Ref(..), Node(..))
import           Luna.Syntax.AST.Term      (Var(..))

import           Empire.Utils.ParserMock   as Parser

type ASTOp = ErrorT Error (NodeBuilderT (Ref Node) (BuilderT AST (StarBuilderT (Maybe (Ref Node)) IO)))

runAstOp :: ASTOp a -> Command AST a
runAstOp cmd = empire $ \g -> flip StarBuilder.evalT Nothing
             $ flip Builder.runT g
             $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
             $ runErrorT
             $ cmd

addNode :: String -> Command AST (Ref Node)
addNode expr = fromMaybe (addVar expr) (whenString <|> whenInt) where
    whenString = addString  <$> Parser.asString  expr
    whenInt    = addInteger <$> Parser.asInteger expr

addInteger :: Int -> Command AST (Ref Node)
addInteger num = runAstOp $ Builder._int num

addVar :: String -> Command AST (Ref Node)
addVar name = runAstOp $ Builder.var name

addString :: String -> Command AST (Ref Node)
addString lit = runAstOp $ Builder._string lit

makeAccessor :: Ref Node -> Ref Node -> Command AST (Ref Node)
makeAccessor src dst = runAstOp $ Builder.accessor dst src

makeApplication :: Ref Node -> [Ref Node] -> Command AST (Ref Node)
makeApplication fun args = runAstOp $ Builder.app fun (Builder.arg <$> args)

getNameNode :: Ref Node -> Command AST (Ref Node)
getNameNode ref = runAstOp $ do
    node <- Builder.readRef ref
    case' (uncoat node) $ do
        match $ \(Var n) -> do
            Builder.follow n
        match $ \ANY -> do
            throwError "Expected Var node, got wrong type."
