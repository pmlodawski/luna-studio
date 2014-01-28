---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
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
import           Flowbox.Luna.Data.AST.Module                           (Module)
import qualified Flowbox.Luna.Data.AST.Module                           as Module
import qualified Flowbox.Luna.Data.AST.Pat                              as Pat
import qualified Flowbox.Luna.Data.AST.Type                             as Type
import           Flowbox.Luna.Data.Pass.ASTInfo                         (ASTInfo)
import qualified Flowbox.Luna.Data.Pass.ASTInfo                         as ASTInfo
import           Flowbox.Luna.Data.Pass.SourceMap                       (SourceMap)
import           Flowbox.Luna.Data.Source                               (Source (Source))
import qualified Flowbox.Luna.Data.Source                               as Source
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Indent
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer      as L
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState (ParseState)
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState as ParseState
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Token      as Token
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Utils
import           Flowbox.Prelude                                        hiding (id, mod)
import qualified Flowbox.Prelude                                        as Prelude

import Control.Monad.State hiding (mapM)
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

getASTInfo = view ParseState.info <$> getState

putASTInfo info = modifyState (ParseState.info .~ info)

genID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID

tok a = a <$> genID

--genID = do
--    st <- getState
--    let id = view ParseState.id st
--    setState (set ParseState.id (id+1) st)
--    pure id


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


pTypeAlias      = tok Expr.TypeAlias <*  L.pTypeAlias
                                     <*> pType
                                     <*  L.pAssignment
                                     <*> pType


pTypeDef        = tok Expr.TypeDef   <*  L.pTypeDef
                                     <*> pType
                                     <*  L.pAssignment
                                     <*> pType


pArg            = tok Expr.Arg      <*> pArgPattern
                                    <*> ((Just <$ L.pAssignment <*> pExpr) <|> pure Nothing)

pFunc           = tok Expr.Function <*  L.pDef
                                    <*> (pExtPath            <?> "")
                                    <*> (pVar <|> L.operator <?> "function name")
                                    <*> (pArgList pArg       <?> "function argument list")
                                    <*> (try (L.pArrow *> pType) <|> tok Type.Unknown)
                                    <*> (pExprBlock <|> return [])
                                    <?> "function definition"


pLambda          = tok Expr.Lambda  <*> pArgListL pArg
                                    <*> (try (L.pArrow *> pType) <|> tok Type.Unknown)
                                    <*> pExprBlock
                                    <?> "lambda definition"


--pData            = do L.pClass
--                      name  <- pCon                 <?> "class name"
--                      tvars <- many L.pIdentTypeVar <?> "class parameters"
--                      cls   <- tok Type.Data <*> pure name <*> pure tvars
--                      pDotBlockBegin pDataBody'
--                      --cons  <- try(pPipeBlockBegin pConD) <|> ((:[]) <$> pConDN name)
--                      tok Expr.Data <*> pure cls <*> pure cons


pData            = Expr.afterData <$> pDataT

pDataT           = tok Data.mk    <*  L.pClass
                                  <*> (tok Type.Data <*> (pCon                 <?> "class name")
                                                     <*> (many L.pIdentTypeVar <?> "class parameters"))
                                  <*> (tok Expr.ConD <*> pure "default" <*> pure [] ) -- default constructor
                                  <??$> pDotBlockBegin pDataBody
                                  <?> "class definition"


--pConDN      name = tok (\i -> Expr.ConD i name [] [] [])
--                                    <??$> pDotBlockBegin pDataBody
--                                    <?> "data constructor definition"

pConD            = tok Expr.ConD    <*> pCon
                                    <*> pure []
                                    <??$> pDotBlockBegin pConDBody
                                    <?> "data constructor definition"


pConDBody        = pCombine Expr.addField pFields


pModule name     = tok Module.mk    <*>   (tok Type.Module <*> pure name)
                                    <??$> withPos (multiBlock1 pModuleBody)



pDataBody       = choice [ Expr.addMethod <$> pFunc
                         , pCombine Expr.addFieldDC pFields
                         , Expr.addClass  <$> pData
                         , Expr.addCon    <$> pConD
                         ]
                <?> "class body"

pModuleBody      = choice [ Module.addMethod    <$> pFunc
                          , pCombine Module.addField pFields
                          , Module.addClass     <$> pData
                          , Module.addImport    <$> pImport
                          , Module.addTypeAlias <$> pTypeAlias
                          , Module.addTypeDef   <$> pTypeDef
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
               <$> (sepBy1 L.pIdentVar L.separator)
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

pExpr       = pExprT pEntBaseE

pExprSimple = pExprT pEntBaseSimpleE

pExprT base =   --(try (tok Expr.RecordUpdate <*> pVar <*> many1 (L.pAccessor *> pVar) <* (L.reservedOp "=")) <*> pOpTE base)
            (try (tok Expr.Assignment   <*> pPattern <* (L.reservedOp "=")) <*> pOpTE base)
            <|> pOpTE base
            <?> "expression"


pOpE       = pOpTE pEntBaseE
pOpTE base = Expr.aftermatch <$> PExpr.buildExpressionParser optableE (pTermE base)

pTermE base = base <??> (flip applyAll <$> many1 (pTermBaseE base))  --  many1 (try $ pTermRecUpd))


pTermBaseE p = choice [ try pTermRecUpd
                      , pDotTermE
                      , pCallTermE p
                      ]

pDotTermBase  = (L.pAccessor *> pVar)

pTermRecUpd   = tok (\id sel expr src -> Expr.RecordUpdate id src sel expr) <*> many1 pDotTermBase <* L.pAssignment <*> pExprSimple

pDotTermE     = try(tok Expr.Accessor <*> pDotTermBase) -- needed by the syntax [1..10]


--pDotTermE   = do
--    exprs   <- fmap (flip Expr.Accessor) <$> pDotTermBase
--    exprsid <- mapM tok exprs
--    return (\x -> foldl (flip ($)) x exprsid)


pCallTermE p = pLastLexemeEmpty *> ((flip <$> tok Expr.App) <*> pCallList p)


pEntBaseE       = pEntConsE pEntComplexE
pEntBaseSimpleE = pEntConsE pEntSimpleE

pEntConsE base = choice [ try $ L.parensed (pExprT base)
                        , base
                        ]

pEntComplexE = choice[ pDeclaration
                     , pEntSimpleE
                     ]
             <?> "expression term"

pEntSimpleE = choice[ pCaseE -- CHECK [wd]: removed try
                    , pCondE
                    , try $ L.parensed pExpr
                    , pIdentE
                    , tok Expr.Lit    <*> pLit
                    , tok Expr.Tuple  <*> pTuple  pOpE
                    , tok Expr.List   <*> pList   pListExpr
                    , tok Expr.Native <*> pNative
                    ]
           <?> "expression term"

optableE = [ [ postfixM  "::" (tok Expr.Typed <*> pType)                      ]
           , [ binaryM   ""   (tok Expr.callConstructor)      PExpr.AssocLeft ]
           , [ operator2 "^"                                  PExpr.AssocLeft ]
           , [ operator2 "*"                                  PExpr.AssocLeft ]
           , [ operator2 "/"                                  PExpr.AssocLeft ]
           , [ operator2 "+"                                  PExpr.AssocLeft ]
           , [ operator2 "-"                                  PExpr.AssocLeft ]
           , [ operator2 "<"                                  PExpr.AssocLeft ]
           , [ operator2 ">"                                  PExpr.AssocLeft ]
           , [ operator2 "=="                                 PExpr.AssocLeft ]
           , [ operator3 "in"                                 PExpr.AssocLeft ]
           , [ binaryM  "$"  (binaryMatchE <$> tok Expr.callConstructor)      PExpr.AssocLeft ]
           ]
           where
              operator op = binaryM op (binaryMatchE <$> (tok Expr.Infix <*> pure ('~':op)))
              --operator2 op = binaryM op (binaryMatchE <$>  ( tok Expr.App <*> (tok Expr.Accessor <*> pure "add" <*> ... ) )  )
              operator2 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID) )
              operator3 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 "contains" y) [x]) <$> genID <*> genID) )


binaryM2  name fun assoc = PExpr.Infix   (L.reserved name *>        fun) assoc

binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)



pVarE   = tok Expr.Var <*> pVar
pConE   = tok Expr.Con <*> pCon

pIdentE = choice [ pVarE
                 , pConE
                 ]

pListExpr = choice [ try $ tok Expr.RangeFromTo <*> pOpE <* L.pRange <*> pOpE
                   , try $ tok Expr.RangeFrom   <*> pOpE <* L.pRange
                   , pOpE
                   ]


pCaseE     = tok Expr.Case <* L.pCase <*> pExprSimple <*> (pDotBlockBegin pCaseBodyE <|> return [])
pCaseBodyE = tok Expr.Match <*> pPattern <*> pExprBlock


pCondE     = tok Expr.Cond <* L.pIf <*> pExprSimple <*> pExprBlock <*> pMaybe (blockSpacesIE *> L.pElse *> pExprBlock)


pNative     = between L.pNativeSym L.pNativeSym (many pNativeElem)
pNativeElem = choice [ pNativeVar
                     , pNativeCode
                     ]
pNativeCode = tok Expr.NativeCode <*> ((:) <$> (noneOf "`#") <*> pNativeCodeBody)
pNativeVar  = tok Expr.NativeVar  <*  L.symbols "#{" <*> many (noneOf "}") <* L.symbol '}'

pNativeCodeBody = (try(lookAhead $ string "#{")  *> pure [])
              <|> (try(lookAhead $ string "```") *> pure [])
              <|> ((++) <$> ((:) <$> anyChar <*> many (noneOf "`#")) <*> pNativeCodeBody)


pExprBlock  = pDotBlockBegin pExpr


-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pTypeSingle = choice [ try $ pLambdaT
                     , pTermT
                     ]
              <?> "type"


pType       = choice [ try $ pConAppT
                     , pTypeSingle
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

pArgPattern = pTermBase pTypeSingle

pTermP      = pTermBase pType

pTermBase t = choice [ try $ L.parensed pPatCon
                     , try (tok Pat.Typed <*> pEntP <* L.pTypeDecl <*> t)
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

pBlockPrefix      = L.pSpaces *> indented

blockSpaces       = try (L.pSpaces <* checkIndent) <|> pure mempty

blockSpacesIE     = try (L.pSpaces <* checkIndentedOrEqual) <|> pure mempty

blockSpaces'      = try (L.pSpaces <* indented) <|> pure mempty

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

pProgEnd = (L.pSpaces <?> "") <* eof

pProgWithState p  = (,) <$> (p <* pProgEnd) <*> getState


type Parser input output = ParsecT input ParseState (State SourcePos) output

pProgram :: [String] -> ParsecT String ParseState (State SourcePos) Module
pProgram mod = L.pSpaces *> pModule mod <* pProgEnd


pResult mod = (\ast st -> (ast, st ^. ParseState.sourceMap, st ^. ParseState.info )) <$> pProgram mod <*> getState

parseGen :: Parser String a -> String -> ASTInfo -> Either ParseError a
parseGen p src astInfo = fst $ flip runState (initialPos "") $ runParserT p (ParseState.mk astInfo) "Luna Parser" src

parse :: Source -> Either ParseError (Module, SourceMap, ASTInfo)
parse (Source mod code) = parseGen (pResult mod) code def

parseExpr    = parseGen (pProgWithState pExpr)
parsePattern = parseGen (pProgWithState pPattern)
parseType    = parseGen (pProgWithState pType)






