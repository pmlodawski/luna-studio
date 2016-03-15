{-# LANGUAGE OverloadedStrings #-}

module Empire.ASTOps.Parse where

import           Prologue

import           Control.Monad.Error          (throwError)

import           Empire.Data.AST              (NodeRef)
import           Empire.ASTOp                 (ASTOp)

import           Empire.API.Data.DefaultValue (PortDefault (..), Value (..))

import qualified Luna.Syntax.Model.Network.Builder as Builder

import qualified Luna.Parser.Parser as Parser
import qualified Luna.Parser.State  as Parser
import qualified Luna.Parser.Term   as Term

parseExpr :: ASTOp m => String -> m NodeRef
parseExpr s = do
    let parsed = Parser.parseString s $ Parser.parseGen Term.partial Parser.defState
    case parsed of
        Left d          -> throwError $ "Parser error: " <> show d
        Right (bldr, _) -> bldr

parsePortDefault :: ASTOp m => PortDefault -> m NodeRef
parsePortDefault (Expression expr)          = parseExpr expr
parsePortDefault (Constant (IntValue i))    = Builder.int $ fromIntegral i
parsePortDefault (Constant (StringValue s)) = Builder.str s
parsePortDefault (Constant (DoubleValue d)) = Builder.double d
parsePortDefault (Constant (BoolValue b))   = Builder.cons (fromString $ show b) []
