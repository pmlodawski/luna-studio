---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Passes.Txt2AST.Parser where

import           Flowbox.Prelude                     
import           Control.Applicative                 
import           Text.Parsec                       hiding (parse, many, optional, (<|>))
import qualified Text.Parsec                       as Parsec
import qualified Text.Parsec.Expr                  as PExpr

import           Flowbox.Luna.Passes.Txt2AST.Utils   
import qualified Flowbox.Luna.Passes.Txt2AST.Lexer as L
import qualified Flowbox.Luna.AST.Expr             as Expr
import qualified Flowbox.Luna.AST.Class            as Class
import qualified Flowbox.Luna.AST.Field            as Field
import qualified Flowbox.Luna.AST.Module           as Module
import qualified Flowbox.Luna.AST.Type             as Type
import qualified Flowbox.Luna.AST.Constant         as Constant
import qualified Flowbox.Luna.AST.Import           as Import
import qualified Flowbox.Luna.Data.Source          as Source


-----------------------------------------------------------
-- Entities
-----------------------------------------------------------

pIdent       = Expr.Var <$> L.pIdent
pIdentType   = Expr.Var <$> L.pIdentType
pIdentVar    = Expr.Var <$> L.pIdentVar
pInt         = Constant.Integer <$> L.integerStr
pCharLit     = Constant.Char    <$> L.charLiteral
pStringLit   = Constant.String  <$> L.stringLiteral
pConstant    = Expr.Constant    <$> choice [ pInt
                                           , pCharLit
                                           , pStringLit
                                           ]

pTuple p     = Expr.Tuple       <$> (     try(L.parensed (return () *> optional L.separator) *> pure [])
                                      <|> try(L.parensed (liftList p <* L.separator))
                                      <|>     L.parensed (sepBy2' p L.separator)
                                    )

pTupleBody p = sepBy' p L.separator
pTuplePure p = L.parensed $ pTupleBody p

pEnt i       = choice [ pIdent
                      , pConstant
                      , pTuple (expr i)
                      ]


-----------------------------------------------------------
-- Types
-----------------------------------------------------------

pType     = Type.Type <$> L.pIdentVar 
pTLambda  = Type.Lambda <$> (Type.Tuple <$> pTuplePure (pType)) <*> return Type.Unknown

-----------------------------------------------------------
-- Expression Entities
-----------------------------------------------------------

--pImportPath _     = (Expr.Path <$> L.pPath) <??> (Expr.Named <$ L.pAs <*> L.pIdent)

--pImport i         = (Expr.Import   <$ L.pImport  <*> ((:) <$> pImportPath i) <?*> pImportBlock i)

--pImportBlock i    = try(pBlock pImportPath (i+1)) <|> return []


pImport i         = Import.mk     <$  L.pImport <*> L.pPath <*> (try (Just <$ L.pAs <*> (L.pIdent <?> "import name")) <|> pure Nothing)

pFunc i           = Expr.Function <$  L.pDef 
                                  <*> L.pIdentVar 
                                  <*> pTLambda
                                  <*> (try (pExprBlock i) <|> return [])
                                  <?> "function definition"



--pLambda i         = Expr.Lambda   <$> (pTuplePure (expr i) <|> liftList pIdent)
--                                 <*> pExprBlock i
--                                 <?> "lambda definition"

pClass i          = Class.mk      <$  L.pClass
                                  <*> L.pIdentType 
                                  <*> many L.pIdentTypeVar
                                  <??$> pBlockBegin pClassBody i 
                                  -- <*> (try (pBlockBegin pClassBody i) <|> return [])
                                  <?> "class definition"

pModule name i    = pure (Module.mk name)  <??$> try(pSegmentBegin pModuleBody i)



pClassBody i      = choice [ Expr.addMethod <$> pFunc i
                           , Expr.addField  <$> pField
                           , Expr.addClass  <$> pClass i
                           ]

pModuleBody i     = choice [ pClassBody i
                           , Expr.addImport <$> pImport i
                           ]


pField            = Field.mk <$> L.pIdent <* L.pTypeDecl <*> L.pIdent

pExprEnt i        = choice [ pImport i
                           , pFunc   i
                           , pClass  i
                           --, pLambda i
                           ]
                               

pExprBlock      i = pBlockBegin expr i --L.pBlockBegin *> ( pBlock expr (i+1) <|> (liftList $ expr i) )

--pBlockOpt     p i = (try (pBlockBegin p i) <|> return [])

pBlockBegin   p i = L.pBlockBegin *> pBlock p (i+1)  

pBlock        p i = L.eol *> pSegmentBegin p i <?> "indented block"


-----------------------------------------------------------
-- Operator Utils
-----------------------------------------------------------

binary   name fun assoc = PExpr.Infix   (L.reservedOp name *> return fun) assoc
binaryM  name fun assoc = PExpr.Infix   (L.reservedOp name *>        fun) assoc
prefix   name fun       = PExpr.Prefix  (L.reservedOp name *> return fun)
prefixM  name fun       = PExpr.Prefix  (L.reservedOp name *>        fun)
prefixfM      fun       = PExpr.Prefix  (fun)
postfix  name fun       = PExpr.Postfix (L.reservedOp name *> return fun)
postfixM name fun       = PExpr.Postfix (L.reservedOp name *>        fun)

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
binaryMatch f = \p q -> f (Expr.aftermatch p) (Expr.aftermatch q)

expr i   = Expr.aftermatch <$> PExpr.buildExpressionParser (exprtable i) (exprterm i)
       <?> "expression"

exprterm i  = choice[ try $ pExprEnt i
                    , try $ L.parensed (expr i)
                    , pEnt i
                    ]
          <?> "simple expression"

exprtable i = [ 
              [ binary   "."  (Expr.Accessor)                PExpr.AssocLeft ]
            , [ postfixM "::" (Expr.Typed <$> L.pIdent)                      ]
            , [ binary   ""   (Expr.callConstructor)         PExpr.AssocLeft ]
            , [ binary   "*"  (binaryMatch $ Expr.Infix "*") PExpr.AssocLeft ]
            , [ binary   "+"  (binaryMatch $ Expr.Infix "+") PExpr.AssocLeft ]
            --,  [binary   "="   Expr.Assignment          Expr.AssocLeft]
            , [ prefixfM      (try(binaryMatch Expr.Assignment <$> (pPattern i) <* (L.reservedOp "=" <?> "pattern match")))]
            ]
      

-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------

pWildcard     = Expr.Wildcard     <$  L.pWildcard

pPatEnt     i = choice [ pIdent
                       , pConstant
                       , pTuple (patTerm i)
                       ]

pPattern    i = Expr.Pattern <$> patTerm i

pPatCons    i = Expr.consConstructor <$> pIdentType <*> PExpr.buildExpressionParser patConsTable (patTerm i) <?> "data constructor"

patTerm     i = choice[ try $ L.parensed (patTerm i)
                      , try $ pPatCons i
                      , pPatEnt  i
                      , pWildcard
                      ]
             <?> "pattern term"

patConsTable  = [ [binary   ""   (Expr.callConstructor)      PExpr.AssocLeft]
                ]

-----------------------------------------------------------
-- Nested Segments
-----------------------------------------------------------

pEmptyLines         = do many1 pEmptyLine

pEmptyLine          = try(L.eol *> L.pSpaces1 *> L.eol) <|> L.eol

pCountAtLast    i p = (++) <$> count i p <*> many p

pIndentExact      i = i <$ count i (char ' ')
pIdentAtLast      i = length <$> pCountAtLast i (char ' ')

pSegments       p i = many $ try $ (pEmptyLines *> pSegment p i)

pSegmentBegin   p i = do
                      j <- many pEmptyLines *> pIdentAtLast i
                      (:) <$> p i <*> pSegments p j

pSegment        p i = try (id <$ pIndentExact i <*> p i)


-----------------------------------------------------------
-- Program
-----------------------------------------------------------

--pProgram mod = Expr.Module (Expr.Path mod) <$> (try([] <$ many(L.pSpaces <* L.eol <* L.pSpaces) <* eof) 
--                                         <|> pSegmentBegin expr 0 <* many(L.eol <* L.pSpaces) <* eof)


pProgram mod = pModule mod 0 <* many(L.eol <* L.pSpaces) <* eof

pExprTemp = expr 0 <* many(L.eol <* L.pSpaces) <* eof

--parseExpr input = Parsec.parse pExprTemp "Luna Parser" input

--pProgram = many $ pPattern 0

parse (Source.Source mod code) = Parsec.parse (pProgram mod) "Luna Parser" $ code



                            
                            
