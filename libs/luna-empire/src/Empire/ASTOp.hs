module Empire.ASTOp where

import           Prologue
import           Control.Monad.State
import           Control.Monad.Error (ErrorT, runErrorT)

import           Luna.Syntax.Builder.Star  (StarBuilderT)
import qualified Luna.Syntax.Builder.Star  as StarBuilder
import           Luna.Syntax.Builder.Node  (NodeBuilderT)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder
import           Luna.Syntax.Builder.Class (BuilderT)
import qualified Luna.Syntax.Builder       as Builder
import           Luna.Syntax.Repr.Graph    (Ref(..), Node(..))

import           Empire.Data.AST (AST)
import           Empire.Empire   (Command, Error, empire)

type ASTOp = ErrorT Error (NodeBuilderT (Ref Node) (BuilderT AST (StarBuilderT (Maybe (Ref Node)) IO)))

runASTOp :: ASTOp a -> Command AST a
runASTOp cmd = empire $ \g -> flip StarBuilder.evalT Nothing
             $ flip Builder.runT g
             $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
             $ runErrorT
             $ cmd
