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
import           Text.Parsec         hiding (State, many, optional, parse, (<|>))
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Expr    as PExpr

import qualified Flowbox.Luna.Data.AST.Data                             as Data
import qualified Flowbox.Luna.Data.AST.Expr                             as Expr
import qualified Flowbox.Luna.Data.AST.Lit                              as Lit
import qualified Flowbox.Luna.Data.AST.Module                           as Module
import qualified Flowbox.Luna.Data.AST.Pat                              as Pat
import qualified Flowbox.Luna.Data.AST.Type                             as Type
import qualified Flowbox.Luna.Data.Source                               as Source
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Indent
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer      as L
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState as ParseState
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Token      as Token
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Utils
import           Flowbox.Prelude                                        hiding (id, mod)
import qualified Flowbox.Prelude                                        as Prelude

import Control.Monad.State
import Text.Parsec.Pos

import qualified Prelude

import Debug.Trace

-----------------------------------------------------------
-- Entities
-----------------------------------------------------------
pTuple     p = L.braced (sepBy p L.separator)
pImplTuple p = sepBy2 p L.separator
pCallList  p = L.parensed (sepBy p L.separator)
pArgListL  p = try(L.parensed (sepBy2 p L.separator)) <|> ((:[]) <$> (try p <|> L.parensed p))
pArgList   p = try(L.parensed (sepBy2 p L.separator)) <|> many (try p <|> L.parensed p)
pArgList'  p = try(L.parensed (sepBy2 p L.separator)) <|> ((:[]) <$> p)
pList      p = L.bracketed (sepBy p L.separator)
pPath1     p = sepBy1_ng p L.pAccessor
pCon         = L.pIdentType
pVar         = L.pIdentVar
pIdent       = choice [ pCon, pVar ]
pExtPath     = (pPath1 pCon <* L.pAccessor) <|> pure []

-----------------------------------------------------------
-- Literals
-----------------------------------------------------------
pIntL    = tok Lit.Integer <*> L.integerStr
pFloatL  = tok Lit.Float   <*> L.floatStr
pCharL   = tok Lit.Char    <*> L.charLiteral
pStringL = tok Lit.String  <*> L.stringLiteral
pLit     = choice [ try $ pFloatL
                  , pIntL
                  , pCharL
                  , pStringL
                  ]

tok a = do
    st <- getState
    let id = view ParseState.id st
    setState (set ParseState.id (id+1) st)
    a <$> pure id

genID = do
    st <- getState
    let id = view ParseState.id st
    setState (set ParseState.id (id+1) st)
    pure id


-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
pImport          = tok Expr.Import   <*  L.pImport
                                     <*> pPath1 pIdent
                                     <*  L.pBlockBegin
                                     <*> (try (tok Expr.Wildcard <* L.pImportAll) <|> pIdentE)
                                     <*> (     try (Just <$ L.pAs <*> (L.pIdent <?> "import name"))
                                           <|> pure Nothing
                                         )


pArg            = tok Expr.Arg      <*> pPatCon
                                    <*> ((Just <$ L.pAssignment <*> pExpr) <|> pure Nothing)

pFunc           = tok Expr.Function <*  L.pDef
                                    <*> (pExtPath       <?> "")
                                    <*> (pVar           <?> "function name")
                                    <*> (pArgList pArg  <?> "dunction argument list")
                                    <*> (try (L.pArrow *> pType) <|> tok Type.Unknown)
                                    <*> (pExprBlock <|> return [])
                                    <?> "function definition"


pLambda          = tok Expr.Lambda  <*> pArgListL pArg
                                    <*> (try (L.pArrow *> pType) <|> tok Type.Unknown)
                                    <*> pExprBlock
                                    <?> "lambda definition"


pData            = do L.pClass
                      name  <- pCon                 <?> "class name"
                      tvars <- many L.pIdentTypeVar <?> "class parameters"
                      cls   <- tok Type.Data <*> pure name <*> pure tvars
                      cons  <- try(pPipeBlockBegin pConD) <|> ((:[]) <$> pConDN name)
                      tok Expr.Data <*> pure cls <*> pure cons

--pData            = tok Expr.Data    <*  L.pClass
--                                    <*> (tok Type.Data  <*> (pCon                 <?> "class name")
--                                                        <*> (many L.pIdentTypeVar <?> "class parameters"))
--                                    <*> (pPipeBlockBegin pConD <|> (pConDN)
--                                    -- <??$> pDotBlockBegin pDataBody
--                                    <?> "class definition"


pConDN      name = tok (\i -> Expr.ConD i name [] [] [])
                                    <??$> pDotBlockBegin pDataBody
                                    <?> "data constructor definition"

pConD            = tok Expr.ConD    <*> pCon
                                    <*> pure []
                                    <*> pure []
                                    <*> pure []
                                    <??$> pDotBlockBegin pDataBody
                                    <?> "data constructor definition"


pModule name     = tok Module.mk    <*>   (tok Type.Module <*> pure name)
                                    <??$> withPos (multiBlock1 pModuleBody)



pDataBody       = choice [ Expr.addMethod <$> pFunc
                          , pCombine Expr.addField pFields
                          , Expr.addClass  <$> pData
                          ]
                <?> "class body"

pModuleBody      = choice [ Module.addMethod <$> pFunc
                          , pCombine Module.addField pFields
                          , Module.addClass  <$> pData
                          , Module.addImport <$> pImport
                          ]
                <?> "module body"

pCombine f p = do
    fs <- map (\x -> f <$> x) <$> p
    foldl (liftA2 (.)) (pure Prelude.id) fs


pField         =   tok Expr.Field
               <*> L.pIdent
               <*  L.pTypeDecl
               <*> pType
               <*> (L.pAssignment *> (Just <$> pExpr) <|> pure Nothing)

mkField t val name id = Expr.Field id name t val

pFields        =   (\names t val -> map (tok . (mkField t val)) names )
               <$> (sepBy1 L.pIdent L.separator)
               <*  L.pTypeDecl
               <*> pType
               <*> (L.pAssignment *> (Just <$> pExpr) <|> pure Nothing)

pDeclaration   = choice [ pImport
                        , pFunc
                        , pData
                        , try $ pLambda
                        ]


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------

pNative     = between L.pNativeSym L.pNativeSym (many pNativeElem)
pNativeElem = choice [ pNativeVar
                     , pNativeCode
                     ]
pNativeVar  = tok Expr.NativeCode <*> many1 (noneOf "`#")
pNativeCode = tok Expr.NativeVar  <*  L.symbols "#{" <*> many (noneOf "}") <* L.symbol '}'

pExpr       = pExprT (pEntBaseConsE pEntBaseExE)

pExprSimple = pExprT (pEntBaseConsE pEntBaseE)

pExprT base =   try (tok Expr.Assignment <*> pPattern <* (L.reservedOp "=") <*> pOpE base)
            <|> pOpE base
            <?> "expression"


pOpE base = Expr.aftermatch <$> PExpr.buildExpressionParser optableE (pDotTermE base)

pDotTermE base = (base) <??> (flip applyAll <$> many1 (tok Expr.Accessor <* L.pAccessor <*> pVar))


pCaseE     = tok Expr.Case <* L.pCase <*> pExprSimple <*> (pDotBlockBegin pCaseBodyE <|> return [])
pCaseBodyE = tok Expr.Match <*> pPattern <*> pExprBlock


pEntBaseConsE base = choice [ try $ L.parensed (pExprT base)
                            , base
                            ]

pEntBaseExE = choice[ pDeclaration
                    , pEntBaseE
                    ]
           <?> "expression term"

pEntBaseE = choice[ try $ pCaseE
                  , try $ L.parensed pExpr
                  , pIdentE
                  , tok Expr.Lit    <*> pLit
                  , tok Expr.Tuple  <*> pTuple  pExpr
                  , tok Expr.List   <*> pList   pListExpr
                  , tok Expr.Native <*> pNative
                  ]
           <?> "expression term"

optableE = [ [ postfixM "::" (tok Expr.Typed <*> pType)                      ]
           , [ binaryM  ""   (tok Expr.callConstructor)      PExpr.AssocLeft ]
           , [ operator "^"                                  PExpr.AssocLeft ]
           , [ operator "*"                                  PExpr.AssocLeft ]
           , [ operator "/"                                  PExpr.AssocLeft ]
           , [ operator "+"                                  PExpr.AssocLeft ]
           , [ operator "-"                                  PExpr.AssocLeft ]
           , [ binaryM  "$"  (binaryMatchE <$> tok Expr.callConstructor)      PExpr.AssocLeft ]
           ]
           where
              operator op = binaryM op (binaryMatchE <$> (tok Expr.Infix <*> pure ('~':op)))

binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)



pVarE   = tok Expr.Var <*> pVar
pConE   = tok Expr.Con <*> pCon

pIdentE = choice [ pVarE
                 , pConE
                 ]

pListExpr = choice [ try $ tok Expr.RangeFromTo <*> pExpr <* L.pRange <*> pExpr
                   , try $ tok Expr.RangeFrom   <*> pExpr <* L.pRange
                   , pExpr
                   ]

pExprBlock  = pDotBlockBegin pExpr


-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pType       = choice [ try $ pLambdaT
                     , try $ pConAppT
                     , pTermT
                     ]
              <?> "type"

pTermT      = choice[ try $ L.parensed pType
                    , pEntT
                    ]
              <?> "type term"

pConAppT    = tok Type.App     <*> pAppBaseT <*> many1 pTermT
pLambdaT    = tok Type.Lambda  <*> pArgList' pTermT <* L.pArrow <*> pTermT
pVarT       = tok Type.Var     <*> pVar
pConT       = tok Type.Con     <*> pPath1 pCon
pTupleT     = tok Type.Tuple   <*> pTuple pType
pWildcardT  = tok Type.Unknown <*  L.pWildcard
--pLambdaT    i   = Type.Lambda <$> pTupleT i <*> return Type.Unknown

pAppBaseT   = choice [ pVarT
                     , pConT
                     ]

pEntT       = choice [ pVarT
                     , pConT
                     , pTupleT
                     , pWildcardT
                     ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pPattern    = choice [ try $ tok Pat.Tuple <*> sepBy2 pPatCon L.separator
                     , pPatCon
                     ]

pPatCon     = choice [ try pConAppP
                     , pTermP
                     ]

pTermP      = choice [ try $ L.parensed pPatCon
                     , try (tok Pat.Typed <*> pEntP <* L.pTypeDecl <*> pType)
                     , pEntP
                     ]
              <?> "pattern term"

pVarP       = tok Pat.Var      <*> pVar
pLitP       = tok Pat.Lit      <*> pLit
pTupleP     = tok Pat.Tuple    <*> pTuple pPatCon
pWildcardP  = tok Pat.Wildcard <*  L.pWildcard
pConP       = tok Pat.Con      <*> pCon
pConAppP    = tok Pat.App      <*> pConP <*> many1 pTermP

pEntP = choice [ pVarP
               , pLitP
               , pTupleP
               , pWildcardP
               , pConP
               ]

-------------------------------------------------------------
---- Nested Segments
-------------------------------------------------------------

pDotBlockBegin  p = L.pBlockBegin *> pBlockBegin p

--pPipeBlockBegin p = pBlockPrefix *> withPos((:) <$ L.pAssignment <*> (p <* blockSpaces) <*> multiBlock (L.pPipe *> p))

--pPipeBlockBegin p = pBlockPrefix *> withPos((:) <$ L.pAssignment <*> (p <* blockSpaces) <*> many ( (same <|> checkIndent) >> (L.pPipe *> p <* blockSpaces)) )

pPipeBlockBegin p = pBlockPrefix *> ((:) <$ L.pAssignment <*> (p <* blockSpaces') <*> many ( (same <|> indented) >> (L.pPipe *> p <* blockSpaces')) )

pBlockBegin     p = pBlockPrefix *> withPos(multiBlock1 p)

pBlockPrefix      = spaces *> indented

blockSpaces       = try (spaces <* checkIndent) <|> pure ()

blockSpaces'      = try (spaces <* indented) <|> pure ()

multiBlock1     p = rawBlock1 (p <* blockSpaces)
multiBlock      p = rawBlock  (p <* blockSpaces)

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

pProgEnd = (spaces <?> "") <* eof

pProgWithState p  = (,) <$> (p <* pProgEnd) <*> getState

parseExpr    = parseGen (pProgWithState pExpr)
parsePattern = parseGen (pProgWithState pPattern)
parseType    = parseGen (pProgWithState pType)


pProgram :: [String] -> ParsecT String ParseState.ParseState (State SourcePos) Module.Module
pProgram mod = spaces *> pModule mod <* pProgEnd


pResult mod = (\ast st -> (ast, view ParseState.sourceMap st)) <$> pProgram mod <*> getState


parse (Source.Source mod code) = parseGen (pResult mod) code def


parseGen p src startID = fst $ flip runState (initialPos "") $ runParserT p startID "Luna Parser" src





