---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Parser where

import           Control.Applicative                
import           Text.Parsec                      hiding (parse, many, optional, (<|>))
import qualified Text.Parsec                      as Parsec
import qualified Text.Parsec.Expr                 as Expr

import           Flowbox.Luna.Parser.Utils          
import qualified Flowbox.Luna.Parser.Lexer        as L
import qualified Flowbox.Luna.Parser.AST.AST      as AST
import qualified Flowbox.Luna.Parser.AST.Class    as Class
import qualified Flowbox.Luna.Parser.AST.Field    as Field
import qualified Flowbox.Luna.Parser.AST.Type     as Type
import qualified Flowbox.Luna.Parser.AST.Constant as Constant


import Debug.Trace 

-----------------------------------------------------------
-- Entities
-----------------------------------------------------------

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


-----------------------------------------------------------
-- Expression Entities
-----------------------------------------------------------

pImportPath _     = (AST.Path <$> L.pPath) <??> (AST.Named <$ L.pAs <*> L.pIdent)

pImport i         = (AST.ImportQualified <$ L.pFrom <*> pImportPath i) 
               <?*> (AST.Import   <$ L.pImport  <*> ((:) <$> pImportPath i) <?*> pImportBlock i)

pImportBlock i    = try(pBlock pImportPath (i+1)) <|> return []


pFunc i           = AST.Function <$  L.pDef 
                                 <*> L.pIdentVar 
                                 <*> pTuplePure (expr i)
                                 <*> (try (pExprBlock i) <|> return [])
                                 <?> "function definition"

pLambda i         = AST.Lambda   <$> (pTuplePure (expr i) <|> liftList pIdent)
                                 <*> pExprBlock i
                                 <?> "lambda definition"

pClass i          = Class.mk     <$  L.pClass
                                 <*> L.pIdentType 
                                 <*> many L.pIdentTypeVar
                                 <??$> pBlockBegin pClassBody i 
                                 -- <*> (try (pBlockBegin pClassBody i) <|> return [])
                                 <?> "class definition"



pClassBody i      = choice [ Class.addMethod <$> pFunc i
                           , Class.addField  <$> pField i
                           , pClass i *> error "Nested classes are not yet supported."
                           ]


pField i          = Field.mk <$> L.pIdent <* L.pTypeDecl <*> L.pIdent

pExprEnt i        = choice [ pImport i
                           , pFunc   i
                           , pClass  i
                           , pLambda i
                           ]
                               

pExprBlock      i = pBlockBegin expr i --L.pBlockBegin *> ( pBlock expr (i+1) <|> (liftList $ expr i) )

--pBlockOpt     p i = (try (pBlockBegin p i) <|> return [])

pBlockBegin   p i = L.pBlockBegin *> ( pBlock p (i+1))

pBlock        p i = L.eol *> pSegmentBegin p i


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------

expr i   = AST.aftermatch <$> Expr.buildExpressionParser table (term i)
       <?> "expression"

term i   = choice[ try $ pExprEnt i
                 , try $ L.parensed (expr i)
                 , pEnt i
                 ]
       <?> "simple expression"

table   = [ 
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

pProgram = AST.Program <$> (try([] <$ many(L.pSpaces <* L.eol <* L.pSpaces) <* eof) 
                       <|> pSegmentBegin expr 0 <* many(L.eol <* L.pSpaces) <* eof)

parse input = Parsec.parse pProgram "Luna Parser" input



                            
                            
