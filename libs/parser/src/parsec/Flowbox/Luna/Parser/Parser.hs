---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction, FlexibleInstances, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Parser where

import Control.Applicative
import Data.Char (isSpace)
import Data.Either.Utils (forceEither)
import Data.Monoid
import System.Environment (getArgs)
import Text.Parsec hiding (parse, many, optional, (<|>))
import qualified Text.Parsec as Parsec
import Data.List ( nub, sort )
import Data.Char ( isAlpha, toLower, toUpper, isSpace, digitToInt )
import qualified Text.Parsec.Expr as Expr
import Text.Parsec.Indent

import           Flowbox.Luna.Parser.Utils
import qualified Flowbox.Luna.Parser.Lexer        as L
import qualified Flowbox.Luna.Parser.AST.AST      as AST
import qualified Flowbox.Luna.Parser.AST.Constant as Constant

import qualified Text.Show.Pretty as PP
import System.TimeIt
import Debug.Trace




---------- Entities ----------

pIdent       = AST.Identifier   <$> L.pIdent
pIdentVar    = AST.Identifier   <$> L.pIdentVar
pInt         = Constant.Integer <$> L.integerStr
pCharLit     = Constant.Char    <$> L.charLiteral
pStringLit   = Constant.String  <$> L.stringLiteral
pConstant    = AST.Constant     <$> choice [ pInt
                                           , pCharLit
                                           , pStringLit
                                           ]

pTuple i     = AST.Tuple        <$> (     try(L.parensed (return () *> optional L.separator) *> pure [])
                                      <|> try(L.parensed (liftList (expr i) <* L.separator))
                                      <|>     L.parensed (sepBy2' (expr i) L.separator)
                                    )

pTupleBody p = sepBy' p L.separator
pTuplePure p = L.parensed $ pTupleBody p

pEnt i       = choice [ pIdent
                      , pConstant
                      , pTuple i
                      ]


---------- ExprEnt ----------

pImportPath _     = (AST.Path <$> L.pPath) <??> (AST.Named <$ L.pAs <*> L.pIdent)

pImport i         = (AST.ImportQualified <$ L.pFrom <*> pImportPath i) 
               <?*> (AST.Import   <$ L.pImport  <*> ((:) <$> pImportPath i) <?*> pImportBlock i)

pImportBlock i    = try(pBlock pImportPath (i+1)) <|> return []

--((:) <$> pImportPath i) <<?*> pBlock pImportPath (i+1))
-- (liftList (pImportPath i)) -- <<?*> pBlock pImportPath (i+1)

pFunc i        = AST.Function <$  L.pDef 
                              <*> L.pIdentVar 
                              <*> pTuplePure (expr i)
                              <*> (try (pExprBlock i) <|> return [])

pLambda i     = AST.Lambda    <$> (pTuplePure (expr i) <|> liftList pIdent)
                              <*> pExprBlock i

pClass i       = AST.Class    <$  L.pClass
                              <*> L.pIdentType 
                              <*> many L.pIdentTypeVar
                              <*> (try (pExprBlock i) <|> return [])

pExprEnt i     = choice [ pImport i
                        , pFunc   i
                        , pClass  i
                        , pLambda i
                        ]

                               

pExprBlock i        = L.pBlockBegin *> ( pBlock expr (i+1) <|> (liftList $ expr i) )

pBlock p i          = L.eol *> pSegmentBegin p i


---------- Expressions ----------

expr i   = AST.aftermatch <$> Expr.buildExpressionParser table (term i)
       <?> "expression"

term i   = choice[ try $ pExprEnt i
                 , try $ L.parensed (expr i)
                 , pEnt i
                 ]
       
       -- <|> L.natural
       <?> "simple expression"

table   = [ 
            --[prefix "-" negate, prefix "+" id ]
          --, [postfix "++" (+1)]
          --, [binary "*" (*) Expr.AssocLeft, binary "/" (div) Expr.AssocLeft ]
          --, [binary "+" (+) Expr.AssocLeft, binary "-" (-)   Expr.AssocLeft ]
            [binary   "."   AST.Accessor            Expr.AssocLeft]
          , [postfixf "::" (AST.Typed <$> L.pIdent)               ]
          , [binary   "*"  (AST.Operator "*")       Expr.AssocLeft]
          , [binary   "+"  (AST.Operator "+")       Expr.AssocLeft]
          , [binary   ""    AST.callConstructor     Expr.AssocLeft]
          , [binary   "="   AST.Assignment          Expr.AssocLeft]
          ]
      
binary   name fun assoc = Expr.Infix   (L.reservedOp name *> return fun) assoc
prefix   name fun       = Expr.Prefix  (L.reservedOp name *> return fun)
postfix  name fun       = Expr.Postfix (L.reservedOp name *> return fun)
postfixf name fun       = Expr.Postfix (L.reservedOp name *> fun)


---------- Nested Segments ----------

pEmptyLines         = many1 pEmptyLine

pEmptyLine          = try(L.eol *> L.pSpaces1 *> L.eol) <|> L.eol

pCountAtLast    i p = (++) <$> count i p <*> many p

pIndentExact      i = i <$ count i (char ' ')
pIdentAtLast      i = length <$> pCountAtLast i (char ' ')

pSegments       p i = many $ try $ (pEmptyLines *> pSegment p i)

pSegmentBegin   p i = do
    j <- many pEmptyLines *> pIdentAtLast i
    (:) <$> p i <*> pSegments p j

pSegment        p i = try (id <$ pIndentExact i <*> p i)


example = unlines [ ""
                  , "import Std.Math.Vector"
				  ]

pProgram = try([] <$ many(L.eol <* L.pSpaces) <* eof) <|> (pSegmentBegin expr 0 <* many(L.eol <* L.pSpaces) <* eof)
    --


parse input = Parsec.parse pProgram "Luna Parser" input




tests = [
        --, ("Simple import", "import Std.Math.Vector as Vector", [Import {paths = [Named {name = "Vector", item = Path {segments = ["Std","Math","Vector"]}}]}])
        --, ("Simple from import", "from Std.Math import Vector", [ImportQualified {path = Path {segments = ["Std","Math"]}, imports = Import {paths = [Path {segments = ["Vector"]}]}}])
        --, ("Complex import", 
        --   "import Std.Math.Vector as Vector\
        -- \\n       Std.Math.Scalar as Scalar"
        --                                        ,[Import {paths = [Named {name = "Vector", item = Path {segments = ["Std","Math","Vector"]}},Named {name = "Scalar", item = Path {segments = ["Std","Math","Scalar"]}}]}])
        --, ("Complex from import", 
        --   "from Std.Math import Vector\
        -- \\n                     Scalar"        
        --                                        ,[ImportQualified {path = Path {segments = ["Std","Math"]}, imports = Import {paths = [Path {segments = ["Vector"]},Path {segments = ["Scalar"]}]}}]
        --  )
        --, ("Exception catch", "e:         \
        --                    \\n    a\n", [])
        ----, ("test", "a=b;b=c", []) -- currently unimplemented
        --("Simple comment", "a=b #[c         \
        --                 \\ndalej komentarz \
        --                 \\n#[nested comment\
        --                 \\n#]xx            \
        --                 \\n#]", [])
        --("Simple comment2", "###################", [])

        --("Empty input",    "x: y: x+y"                  , [])  
        --,("Empty input",    "a b"                  , [])  
        --,("Empty input",    "a+b"                  , [])  
        ]


main = do
    timeIt main_inner

main_inner = do
    --args <- getArgs
    --input <- if null args then return example else readFile $ head args
    let out = parse example
    print out
    putStrLn $ PP.ppShow $ out
    --putStrLn "\n--- tests ---\n"
    --mapM_ (run True) tests
    --return ()
    --putStrLn $ serializeIndentedTree $ forceEither $ parseIndentedTree input



run force (title, inp, exp) = 
    do 
        putStr ("=== " ++ title ++ " ===" ++ replicate (30 - length title) ' ')
        let a =  parse $ unlines inp
        case a of
            Right expr -> do 
                if (expr == exp)
                    then putStrLn "ok."
                    else putStrLn $ "FAIL (wrong result)"
                
                if (expr /= exp) || force
                    then do
                        print_err a
                        putStrLn $ PP.ppShow a
                    else return ()
            Left  e   -> do
                putStrLn $ "FAIL (parse fail)"
                print_err a

    where
        print_err a = do
            print a
            putStrLn ""
                            
                            
