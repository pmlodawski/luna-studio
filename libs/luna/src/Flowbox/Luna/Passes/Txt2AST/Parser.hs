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
import qualified Flowbox.Luna.AST.Lit              as Lit
import qualified Flowbox.Luna.AST.Pat              as Pat
import qualified Flowbox.Luna.AST.Class            as Class
import qualified Flowbox.Luna.AST.Field            as Field
import qualified Flowbox.Luna.AST.Module           as Module
import qualified Flowbox.Luna.AST.Type             as Type
import qualified Flowbox.Luna.AST.Import           as Import
import qualified Flowbox.Luna.Data.Source          as Source


-----------------------------------------------------------
-- Entities
-----------------------------------------------------------

pTuple p     = try(L.parensed (return () *> optional L.separator) *> pure [])
           <|> try(L.parensed (liftList p <* L.separator))
           <|>     L.parensed (sepBy2' p L.separator)
               

pTupleBody p = sepBy' p L.separator
pTuplePure p = L.parensed $ pTupleBody p

pType        = sepBy1 L.pIdentType L.pAccessor


-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
pIntL    = Lit.Integer   <$> L.integerStr
pCharL   = Lit.Char      <$> L.charLiteral
pStringL = Lit.String    <$> L.stringLiteral
pLit     = choice [ pIntL
                  , pCharL
                  , pStringL
                  ]


-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pSig      = Type.Sig <$> L.pIdentVar 
pLambdaT  = Type.Lambda <$> (Type.Tuple <$> pTuplePure (pSig)) <*> return Type.Unknown

pEntT     = choice [ Type.Var  <$> L.pIdentVar
                   , Type.Type <$> pType
                   ]


-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
pImport i         = Import.mk     <$  L.pImport <*> L.pPath <*> (try (Just <$ L.pAs <*> (L.pIdent <?> "import name")) <|> pure Nothing)

pFunc i           = Expr.Function <$  L.pDef 
                                  <*> L.pIdentVar 
                                  <*> pLambdaT
                                  <*> (try (pExprBlock i) <|> return [])
                                  <?> "function definition"



--pLambda i         = Expr.Lambda   <$> (pTuplePure (pExpr i) <|> liftList pIdent)
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

pDeclaration i    = choice [ pImport i
                           , pFunc   i
                           , pClass  i
                           --, pLambda i
                           ]


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
binaryMatch f = \p q -> f (Expr.aftermatch p) (Expr.aftermatch q)

pExpr     i   = Expr.aftermatch <$> PExpr.buildExpressionParser (exprtable i) (exprterm i)
           <?> "expression"

exprterm i   = choice[ try $ pDeclaration i
                     , try $ L.parensed (pExpr i)
                     , pEntE i
                     ]
           <?> "expression term"

exprtable i  = [ [ binary   "."  (Expr.Accessor)                PExpr.AssocLeft ]
               , [ postfixM "::" (Expr.Typed <$> pEntT)           ]
               , [ binary   ""   (Expr.callConstructor)         PExpr.AssocLeft ]
               , [ binary   "*"  (binaryMatch $ Expr.Infix "*") PExpr.AssocLeft ]
               , [ binary   "+"  (binaryMatch $ Expr.Infix "+") PExpr.AssocLeft ]
               , [ prefixfM      (try(binaryMatch Expr.Assignment <$> (pPatExpr i) <* (L.reservedOp "=" <?> "pattern match")))]
               ]

pEntE    i   = choice [ Expr.Var   <$> L.pIdent
                      , Expr.Lit   <$> pLit
                      , Expr.Tuple <$> pTuple (pExpr i)
                      ]

pPatExpr  i  = Expr.Pattern <$> pPattern i

pExprBlock      i = pBlockBegin pExpr i --L.pBlockBegin *> ( pBlock pExpr (i+1) <|> (liftList $ pExpr i) )


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
--pPattern    i   = Pat.Cons <$> pType <*> PExpr.buildExpressionParser consTableP (pTermP i) 
--              <?> "pattern"

--consTableP      = [ --[ postfixM "::" (Pat.Typed <$> pEntT)       ]
--                  [ binary   ""   (Pat.callConstructor)      PExpr.AssocLeft ]
--                  ]

pPattern    i   = choice [ pConsP i
                         , pTermP i 
                         ]

pConsP      i   = Pat.Cons <$> pType <*> many (pTermP i) 

pTermP      i   = choice[ try $ L.parensed (pPattern i)
                        , pEntP  i
                        ]
              <?> "pattern term"


pEntP       i   = choice [ Pat.Var      <$> L.pIdentVar
                         , Pat.Lit      <$> pLit
                         , Pat.Tuple    <$> pTuple (pTermP i)
                         , Pat.Wildcard <$  L.pWildcard
                         , Pat.Cons     <$> pType <*> pure []
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

pBlockBegin     p i = L.pBlockBegin *> pBlock p (i+1)  

pBlock          p i = L.eol *> pSegmentBegin p i <?> "indented block"


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
-- Program
-----------------------------------------------------------

--pProgram mod = Expr.Module (Expr.Path mod) <$> (try([] <$ many(L.pSpaces <* L.eol <* L.pSpaces) <* eof) 
--                                         <|> pSegmentBegin pExpr 0 <* many(L.eol <* L.pSpaces) <* eof)


--pProgram mod = pModule mod 0 <* many(L.eol <* L.pSpaces) <* eof

pProgram mod = pPattern 0


pExprTemp = pExpr 0 <* many(L.eol <* L.pSpaces) <* eof

--parseExpr input = Parsec.parse pExprTemp "Luna Parser" input

--pProgram = many $ pPattern 0

parse (Source.Source mod code) = Parsec.parse (pProgram mod) "Luna Parser" $ code



                            
                            
