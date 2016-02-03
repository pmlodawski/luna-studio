module Empire.ASTOps.Parse where

import           Prologue
import           Data.Maybe              (fromMaybe)
import           Data.List               (unfoldr)
import           Control.Monad           (foldM)
import           Control.Monad.Error     (throwError)

import           Empire.ASTOp            (ASTOp)
import           Empire.Utils.ParserMock as Parser
import           Empire.ASTOps.Builder   as ASTBuilder

import           Empire.API.Data.DefaultValue (PortDefault(..), Value(..))

import qualified Luna.Syntax.Model.Graph.Term as Builder
import           Luna.Syntax.Model.Graph  (Ref)

parsePortDefault :: PortDefault -> ASTOp (Ref Node)
parsePortDefault (Expression expr)          = parseFragment expr
parsePortDefault (Constant (IntValue i))    = Builder._int i
parsePortDefault (Constant (StringValue s)) = Builder._string s

parseFragment :: String -> ASTOp (Ref Node)
parseFragment = parseApp

parseApp :: String -> ASTOp (Ref Node)
parseApp expr = do
    appList <- mapM parseAcc $ words expr
    case appList of
        []       -> throwError "Can't parse: empty expression"
        [f]      -> return f
        f : args -> ASTBuilder.reapply f args

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy a = (filter $ not . null) . unfoldr sep where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== a) $ l

parseAcc :: String -> ASTOp (Ref Node)
parseAcc expr = do
    let parts = splitBy '.' expr
    case parts of
        []       -> throwError "Can't parse: empty expression"
        t : accs -> do
            target <- parseInitial t
            as     <- mapM Builder.var accs
            buildAccs target as

buildAccs :: Ref Node -> [Ref Node] -> ASTOp (Ref Node)
buildAccs = foldM ASTBuilder.makeAccessor

parseInitial :: String -> ASTOp (Ref Node)
parseInitial expr = fromMaybe (Builder.var expr) (whenBlank <|> whenString <|> whenInt) where
    whenString = Builder._string <$> Parser.asString  expr
    whenInt    = Builder._int    <$> Parser.asInteger expr
    whenBlank  = if expr == "_" then Just Builder._blank else Nothing
