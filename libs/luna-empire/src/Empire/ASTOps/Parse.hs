{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.Parse where

import           Prologue

import           Control.Monad                (foldM)
import           Control.Monad.Error          (throwError)
import           Data.List                    (unfoldr, break)
import           Data.Maybe                   (fromMaybe)

import           Empire.Data.AST              (ASTEdge, EdgeRef, NodeRef)
import           Empire.ASTOp                 (ASTOp)
import           Empire.ASTOps.Builder        as ASTBuilder
import           Empire.Utils.ParserMock      as Parser

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))
import qualified Luna.Syntax.AST.Term.Lit     as Lit

import qualified Luna.Syntax.Model.Network.Builder as Builder

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = parseFragment expr
parsePortDefault (Constant (IntValue i))    = Builder.int $ fromIntegral i
parsePortDefault (Constant (StringValue s)) = Builder.str s
parsePortDefault (Constant (DoubleValue d)) = Builder.double d

parseFragment :: ASTOp m => String -> m NodeRef
parseFragment = parseApp

parseApp :: ASTOp m => String -> m NodeRef
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

parseAcc :: ASTOp m => String -> m NodeRef
parseAcc expr = do
    let parts = splitBy '.' expr
    case parts of
        []       -> throwError "Can't parse: empty expression"
        t : accs -> do
            target <- parseInitial t
            ASTBuilder.buildAccessors target accs

parseInitial :: ASTOp m => String -> m NodeRef
parseInitial expr = fromMaybe (Builder.var (fromString expr :: Lit.String)) (whenBlank <|> whenString <|> whenInt) where
    whenString = Builder.str <$> Parser.asString  expr
    whenInt    = Builder.int . fromIntegral <$> Parser.asInteger expr
    whenBlank  = if expr == "_" then Just Builder.blank else Nothing
