module Empire.ASTOps.Parse where

import           Prologue
import           Data.Maybe              (fromMaybe)
import           Empire.ASTOp            (ASTOp)
import qualified Luna.Syntax.Builder     as Builder
import           Empire.Utils.ParserMock as Parser
import           Luna.Syntax.Repr.Graph  (Ref, Node)

parseFragment :: String -> ASTOp (Ref Node)
parseFragment expr = fromMaybe (Builder.var expr) (whenString <|> whenInt) where
    whenString = Builder._string <$> Parser.asString  expr
    whenInt    = Builder._int    <$> Parser.asInteger expr
