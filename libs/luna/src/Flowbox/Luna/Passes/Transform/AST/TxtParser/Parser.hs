---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Passes.Transform.AST.TxtParser.Parser where

import           Control.Applicative                                 
import           Text.Parsec                                       hiding (parse, many, optional, (<|>))
import qualified Text.Parsec                                       as Parsec
import qualified Text.Parsec.Expr                                  as PExpr

import qualified Flowbox.Prelude                                   as Prelude
import           Flowbox.Prelude                                   hiding (id, mod)
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Utils   
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer as L
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import qualified Flowbox.Luna.Data.AST.Class                       as Class
import qualified Flowbox.Luna.Data.AST.Module                      as Module
import qualified Flowbox.Luna.Data.AST.Type                        as Type
import qualified Flowbox.Luna.Data.Source                          as Source



-----------------------------------------------------------
-- Entities
-----------------------------------------------------------
pTuple      p = L.braced (sepBy p L.separator)
pImplTuple  p = sepBy2 p L.separator
pCallList s p = L.parensed s (sepBy p L.separator)
pArgListL s p = try(L.parensed s (sepBy2 p L.separator)) <|> ((:[]) <$> (try p <|> L.parensed s p))
pArgList  s p = try(L.parensed s (sepBy2 p L.separator)) <|> many (try p <|> L.parensed s p)
pArgList' s p = try(L.parensed s (sepBy2 p L.separator)) <|> ((:[]) <$> p)
pList       p = L.bracketed (sepBy p L.separator)
pPath1      p = sepBy1' p L.pAccessor
pCon      s   = L.pIdentType s
pVar      s   = L.pIdentVar s
pIdent    s   = choice [ pCon s, pVar s ]

pExtPath  s   = (pPath1 (pCon s) <* L.pAccessor) <|> pure []

-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
pIntL    s = tok Lit.Integer <*> L.integerStr s
pCharL   s = tok Lit.Char    <*> L.charLiteral s
pStringL s = tok Lit.String  <*> L.stringLiteral s
pLit     s = choice [ pIntL    s
                    , pCharL   s
                    , pStringL s
                    ]

tok a = do
    id <- getState
    setState (id+1)
    a <$> pure id

genID = do
    id <- getState
    setState (id+1)
    pure id


-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
pImport     s      = tok Expr.Import   <*  L.pImport 
                                       <*> pPath1 (pIdent s)
                                       <*  L.pBlockBegin
                                       <*> (try (tok Expr.Wildcard <* L.pImportAll) <|> pIdentE s)
                                       <*> (     try (Just <$ L.pAs <*> (L.pIdent s <?> "import name")) 
                                             <|> pure Nothing
                                           )


pArg        s i    = tok Expr.Arg      <*> pPatCon s i 
                                       <*> ((Just <$ L.pAssignment <*> pExpr s i) <|> pure Nothing)

pFunc       s i    = tok Expr.Function <*  L.pDef 
                                       <*> pExtPath s
                                       <*> (pVar s)
                                       <*> pArgList s (pArg s i)
                                       <*> (try (L.pArrow *> pType s i) <|> tok Type.Unknown)
                                       <*> (pExprBlock s i <|> return [])
                                       <?> "function definition"


pLambda      s i    = tok Expr.Lambda  <*> pArgListL s (pArg s i)
                                       <*> (try (L.pArrow *> pType s i) <|> tok Type.Unknown)
                                       <*> pExprBlock s i
                                       <?> "lambda definition"


pClass       s i    = tok Class.mk     <*  L.pClass
                                       <*> (tok Type.Class <*> L.pIdentType s <*> many (L.pIdentTypeVar s))
                                       <??$> pBlockBegin (pClassBody s) i 
                                       <?> "class definition"

pModule name s i    = tok Module.mk    <*>   (tok Type.Module <*> pure name)
                                       <??$> pSegmentBegin (pModuleBody s) i



pClassBody   s i    = choice [ Expr.addMethod <$> pFunc s i
                             , pCombine Expr.addField (pFields s i)
                             , Expr.addClass  <$> pClass s i
                             ]

pModuleBody  s i    = choice [ Module.addMethod <$> pFunc s i
                             , pCombine Module.addField (pFields s i)
                             , Module.addClass  <$> pClass s i
                             , Module.addImport <$> pImport s 
                             ]

pCombine f p = do
    fs <- map (\x -> f <$> x) <$> p
    foldl (liftA2 (.)) (pure Prelude.id) fs


pField       s i    =   tok Expr.Field 
                    <*> L.pIdent s 
                    <*  L.pTypeDecl 
                    <*> pType s i
                    <*> (L.pAssignment *> (Just <$> pExpr s i) <|> pure Nothing)

mkField t val name id = Expr.Field id name t val

pFields      s i    =   (\names t val -> map (tok . (mkField t val)) names )
                    <$> (sepBy1 (L.pIdent s) L.separator)
                    <*  L.pTypeDecl 
                    <*> pType s i
                    <*> (L.pAssignment *> (Just <$> pExpr s i) <|> pure Nothing)

pDeclaration s i    = choice [ pImport s 
                             , pFunc   s i
                             , pClass  s i
                             , try $ pLambda s i
                             ]


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------

pNative         = between L.pNativeSym L.pNativeSym (many pNativeElem)
pNativeElem     = choice [ pNativeVar
                         , pNativeCode
                         ]
pNativeVar      = tok Expr.NativeCode <*> many1 (noneOf "`#")
pNativeCode     = tok Expr.NativeVar  <*  L.symbols "#{" <*> many (noneOf "}") <* L.symbol2 False '}'


pExpr     s i   = Expr.aftermatch <$> PExpr.buildExpressionParser (optableE s i) (pTermE s i)
           <?> "expression"

pTermE    s i   = choice[ pDeclaration s i
                        , try $ L.parensed s (pExpr s i)
                        , pEntE s i
                        ]
           <?> "expression term"

optableE  s i  = [ [ postfixM "."  (tok Expr.Accessor <*> pIdent s)                ]
                 , [ postfixM "::" (tok Expr.Typed <*> pType s i)                  ]
                 , [ binaryM  ""   (tok Expr.callConstructor)      PExpr.AssocLeft ]
                 , [ binaryM  "*"  (binaryMatchE <$> (tok Expr.Infix <*> pure "*")) PExpr.AssocLeft ]
                 , [ binaryM  "+"  (binaryMatchE <$> (tok Expr.Infix <*> pure "+")) PExpr.AssocLeft ]
                 , [ prefixfM      (try(binaryMatchE2 <$> tok Expr.Assignment <*> (pPattern s i) <* (L.reservedOp "=" <?> "pattern match")))]
                 ]

binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)
binaryMatchE2 f p q = f p (Expr.aftermatch q)


pVarE     s   = tok Expr.Var <*> pVar s
pConE     s   = tok Expr.Con <*> pCon s

pIdentE   s   = choice [ pVarE s
                       , pConE s
                       ]

pListExpr s i = choice [ try $ tok Expr.RangeFromTo <*> pExpr s i <* L.pRange <*> pExpr s i
                       , try $ tok Expr.RangeFrom   <*> pExpr s i <* L.pRange
                       , pExpr s i
                       ]

pEntBaseE s i = choice [ pIdentE s
                       , tok Expr.Lit    <*> pLit s
                       , tok Expr.Tuple  <*> pTuple  (pExpr s i)
                       , tok Expr.List   <*> pList   (pListExpr s i)
                       , tok Expr.Native <*> pNative
                       ]

-- Function application using parenthesis notation, e.g. f(1).next <=> (f 1).next or f (1).next <=> f 1.next
pEntE     s i = (\expr ops -> foldr ($) expr $ reverse ops)
             <$> pEntBaseE False i 
             <*> choice [ try $ many1 ( flip <$> (Expr.App <$> genID) <*> pCallList False (pTermE s i))
                        ,       [] <$ L.pSpaces
                        ]

---- Implicit tuples support
--pEntE s i = try(tok Expr.Tuple <*> pImplTuple (pAppE s i)) <|> (pAppE s i)

pExprBlock     s i = pBlockBegin (pExpr s) i


-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pType       s i   = choice [ try $ pLambdaT s i
                           , try $ pConAppT s i
                           , pTermT s i 
                           ]
              <?> "type"

pTermT      s i   = choice[ try $ L.parensed s (pType s i)
                        , pEntT s i
                        ]
              <?> "type term"

pConAppT    s i   = tok Type.App     <*> pAppBaseT s <*> many1 (pTermT s i) 
pLambdaT    s i   = tok Type.Lambda  <*> pArgList' s (pTermT s i) <* L.pArrow <*> pTermT s i
pVarT       s     = tok Type.Var     <*> pVar s
pConT       s     = tok Type.Con     <*> pPath1 (pCon s)
pTupleT     s i   = tok Type.Tuple   <*> pTuple (pType s i)
pWildcardT        = tok Type.Unknown <*  L.pWildcard
--pLambdaT    i   = Type.Lambda <$> pTupleT i <*> return Type.Unknown

pAppBaseT   s     = choice [ pVarT   s 
                           , pConT  s  
                           ]

pEntT       s i   = choice [ pVarT   s 
                           , pConT  s 
                           , pTupleT s i
                           , pWildcardT
                           ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pPattern    s i = choice [ try $ tok Pat.Tuple <*> sepBy2 (pPatCon s i) L.separator
                         , pPatCon s i
                         ]

pPatCon     s i = choice [ try(pConAppP s i)
                         , pTermP s i 
                         ]

pTermP      s i = choice [ try $ L.parensed s (pPatCon s i)
                         , try (tok Pat.Typed <*> pEntP s i <* L.pTypeDecl <*> pType s i)
                         , pEntP s i
                         ]
              <?> "pattern term"

pVarP       s   = tok Pat.Var      <*> pVar s
pLitP       s   = tok Pat.Lit      <*> pLit s
pTupleP     s i = tok Pat.Tuple    <*> pTuple (pPatCon s i)
pWildcardP      = tok Pat.Wildcard <*  L.pWildcard
pConP       s   = tok Pat.Con     <*> pCon s
pConAppP   s i = tok Pat.App      <*> pConP s <*> many1 (pTermP s i) 

pEntP   s i = choice [ pVarP      s
                     , pLitP      s
                     , pTupleP    s i
                     , pWildcardP
                     , pConP      s
                     ]

---- Implicit tuples support
--pEntP s i = try(tok Pat.Tuple <*> pImplTuple (pEntBaseP s i)) <|> (pEntBaseP s i)

-----------------------------------------------------------
-- Nested Segments
-----------------------------------------------------------
pEmptyLines         = many1 pEmptyLine

pEmptyLine          = try(L.eol *> L.pSpaces1 *> L.eol) <|> L.eol

pIndentExact      i = i <$ count i (char ' ')

pIdentAtLast      i = do
                      _   <- many (char ' ')
                      col <- sourceColumn <$> getPosition
                      if col > i then pure (col-1)
                                 else fail "incorrect indentation"

pSegments       p i = many $ try $ (pEmptyLines *> pSegment p i)

pSegmentBegin   p i = do
                      j <- many pEmptyLines *> pIdentAtLast i
                      (:) <$> p i <*> pSegments p j

pSegment        p i = try (Prelude.id <$ pIndentExact i <*> p i)

pBlockBegin     p i = L.pBlockBegin *> pBlock p (i+1)  

pBlock          p i = pSegmentBegin p i <?> "indented block"


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

pExprTemp = do
    out <- pExpr True 0 <* many(L.eol <* L.pSpaces) <* eof
    id  <- getState
    return (out, id)



parseExpr input startID = Parsec.runParser pExprTemp startID "Luna Parser" input

pProgram mod = pModule mod True 0 <* many(L.eol <* L.pSpaces) <* eof



parse (Source.Source mod code) = Parsec.runParser (pProgram mod) (0::Int) "Luna Parser" $ code



                            
                            
