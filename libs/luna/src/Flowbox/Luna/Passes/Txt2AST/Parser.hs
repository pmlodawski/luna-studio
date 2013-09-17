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

pTuple     p = L.braced (sepBy' p L.separator)

pArgList   p = try(L.parensed (sepBy2 p L.separator)) <|> many p
               
pArgList'  p = try(L.parensed (sepBy2 p L.separator)) <|> ((:[]) <$> p)

--pTuple p     = try(L.parensed (return () *> optional L.separator) *> pure [])
--           <|> try(L.parensed (liftList p <* L.separator))
--           <|>     L.parensed (sepBy2' p L.separator)

pList p      = L.bracketed (sepBy' p L.separator)

pTupleBody p = sepBy' p L.separator
pTuplePure p = L.braced $ pTupleBody p

pCons        = sepBy1 L.pIdentType L.pAccessor


-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
pIntL    = tok Lit.Integer <*> L.integerStr
pCharL   = tok Lit.Char    <*> L.charLiteral
pStringL = tok Lit.String  <*> L.stringLiteral
pLit     = choice [ pIntL
                  , pCharL
                  , pStringL
                  ]


tok a = do
    id <- getState
    setState (id+1)
    a <$> pure id

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
pImport i         = tok Import.mk     <*  L.pImport <*> L.pPath <*> (try (Just <$ L.pAs <*> (L.pIdent <?> "import name")) <|> pure Nothing)

pFunc i           = tok Expr.Function <*  L.pDef 
                                      <*> L.pIdentVar 
                                      <*> pArgList (pPattern i)
                                      <*> (pExprBlock i <|> return [])
                                      <?> "function definition"



--pLambda i         = Expr.Lambda   <$> (pTuplePure (pExpr i) <|> liftList pIdent)
--                                 <*> pExprBlock i
--                                 <?> "lambda definition"

pClass i          = tok Class.mk  <*  L.pClass
                                  <*> (tok Type.Class <*> L.pIdentType <*> many L.pIdentTypeVar)
                                  <??$> pBlockBegin pClassBody i 
                                  -- <*> (try (pBlockBegin pClassBody i) <|> return [])
                                  <?> "class definition"

pModule name i    = tok Module.mk <*>   (tok Type.Module <*> pure name)
                                  <??$> try(pSegmentBegin pModuleBody i)



pClassBody i      = choice [ Expr.addMethod <$> pFunc i
                           , Expr.addField  <$> pField i
                           , Expr.addClass  <$> pClass i
                           ]

pModuleBody i     = choice [ pClassBody i
                           , Expr.addImport <$> pImport i
                           ]


pField       i    = tok Field.mk <*> L.pIdent <* L.pTypeDecl <*> pType i

pDeclaration i    = choice [ pImport i
                           , pFunc   i
                           , pClass  i
                           --, pLambda i
                           ]


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------

pExpr     i   = Expr.aftermatch <$> PExpr.buildExpressionParser (optableE i) (pTermE i)
           <?> "expression"

pTermE    i   = choice[ try $ pDeclaration i
                     , try $ L.parensed (pExpr i)
                     , pEntE i
                     ]
           <?> "expression term"

optableE  i  = [ [ binaryM  "."  (tok Expr.Accessor)             PExpr.AssocLeft ]
               , [ postfixM "::" (tok Expr.Typed <*> pType i)                    ]
               , [ binaryM   ""  (tok Expr.callConstructor)      PExpr.AssocLeft ]
               , [ binaryM  "*"  (binaryMatchE <$> (tok Expr.Infix <*> pure "*")) PExpr.AssocLeft ]
               , [ binaryM  "+"  (binaryMatchE <$> (tok Expr.Infix <*> pure "+")) PExpr.AssocLeft ]
               , [ prefixfM      (try(binaryMatchE2 <$> tok Expr.Assignment <*> (pPattern i) <* (L.reservedOp "=" <?> "pattern match")))]
               ]

binaryMatchE  f p q = f (Expr.aftermatch p) (Expr.aftermatch q)
binaryMatchE2 f p q = f p (Expr.aftermatch q)

pEntE    i   = choice [ tok Expr.Var   <*> L.pIdentVar
                      , tok Expr.Cons  <*> pCons
                      , tok Expr.Lit   <*> pLit
                      , tok Expr.Tuple <*> pTuple (pExpr i)
                      , tok Expr.List  <*> pList (pExpr i)
                      ]

pExprBlock      i = pBlockBegin pExpr i --L.pBlockBegin *> ( pBlock pExpr (i+1) <|> (liftList $ pExpr i) )



-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pType       i   = choice [ try $ pLambdaT i
                         , try $ pConsAppT i
                         , pTermT i 
                         ]
              <?> "type"

pTermT      i   = choice[ try $ L.parensed (pType i)
                        , pEntT i
                        ]
              <?> "type term"

pVarT       i   = tok Type.Var    <*> L.pIdentVar
pConsT      i   = tok Type.Cons   <*> pCons
pTupleT     i   = tok Type.Tuple  <*> pTuple (pType i)
pConsAppT   i   = tok Type.App    <*> pConsT i <*> many1 (pTermT i) 
pLambdaT    i   = tok Type.Lambda <*> pArgList' (pTermT i) <* L.pArrow <*> pArgList' (pTermT i)
--pLambdaT    i   = Type.Lambda <$> pTupleT i <*> return Type.Unknown

pEntT       i   = choice [ pVarT   i
                         , pTupleT i
                         , pConsT  i
                         ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pPattern    i   = choice [ try(pConsAppP i)
                         , pTermP i 
                         ]

pTermP      i   = choice[ try $ L.parensed (pPattern i)
                        , try (tok Pat.Typed <*> pEntP i <* L.pTypeDecl <*> pType i)
                        , pEntP i
                        ]
              <?> "pattern term"

pVarP           = tok Pat.Var      <*> L.pIdentVar
pLitP           = tok Pat.Lit      <*> pLit
pTupleP     i   = tok Pat.Tuple    <*> pTuple (pTermP i)
pWildcardP      = tok Pat.Wildcard <*  L.pWildcard
pConsP          = tok Pat.Cons     <*> pCons
pConsAppP   i   = tok Pat.App      <*> (tok Pat.Cons <*> pCons) <*> many1 (pTermP i) 

pEntP       i   = choice [ pVarP
                         , pLitP
                         , pTupleP i
                         , pWildcardP
                         , pConsP
                         ]

--pLambdaP    i   = 


-----------------------------------------------------------
-- Nested Segments
-----------------------------------------------------------
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
--                                           <|> pSegmentBegin pExpr 0 <* many(L.eol <* L.pSpaces) <* eof)


pProgram mod = pModule mod 0 <* many(L.eol <* L.pSpaces) <* eof

--pProgram mod = pPattern 0


pExprTemp = pExpr 0 <* many(L.eol <* L.pSpaces) <* eof

parseExpr input startID = Parsec.runParser pExprTemp startID "Luna Parser" input

--pProgram = many $ pPattern 0

parse (Source.Source mod code) = Parsec.runParser (pProgram mod) (0::Int) "Luna Parser" $ code



                            
                            
