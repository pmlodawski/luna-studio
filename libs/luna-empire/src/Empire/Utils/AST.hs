module Empire.Utils.AST where

import           Prologue
import           Empire.Data.AST (AST)

import           Luna.Syntax.Builder.Star  (StarBuilder)
import qualified Luna.Syntax.Builder.Star  as StarBuilder
import           Luna.Syntax.Builder.Node  (NodeBuilderT)
import qualified Luna.Syntax.Builder.Node  as NodeBuilder
import           Luna.Syntax.Builder.Class (BuilderT)
import qualified Luna.Syntax.Builder       as Builder
import           Luna.Syntax.Repr.Graph    (Ref(..), Node(..))

type ASTOp = NodeBuilderT (Ref Node) (BuilderT AST (StarBuilder (Maybe (Ref Node))))

runASTOp :: ASTOp a -> AST -> (a, AST)
runASTOp cmd g = runIdentity
               $ flip StarBuilder.evalT Nothing
               $ flip Builder.runT g
               $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
               $ cmd

addVar :: String -> AST -> (Ref Node, AST)
addVar name g = flip runASTOp g $ Builder.var name
