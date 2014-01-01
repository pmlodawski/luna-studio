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
import           Text.Parsec         hiding (many, optional, parse, (<|>), State)
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Expr    as PExpr

import qualified Flowbox.Luna.Data.AST.Class                       as Class
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import qualified Flowbox.Luna.Data.AST.Lit                         as Lit
import qualified Flowbox.Luna.Data.AST.Module                      as Module
import qualified Flowbox.Luna.Data.AST.Pat                         as Pat
import qualified Flowbox.Luna.Data.AST.Type                        as Type
import qualified Flowbox.Luna.Data.Source                          as Source
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Lexer as L
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Utils
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Indent
import           Flowbox.Luna.Passes.Transform.AST.TxtParser.Token (Token(Token))
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.Token as Token
import qualified Flowbox.Luna.Passes.Transform.AST.TxtParser.ParseState as ParseState
import           Flowbox.Prelude                                   hiding (id, mod)
import qualified Flowbox.Prelude                                   as Prelude

import Text.Parsec.Pos
import Control.Monad.State

import qualified Prelude

import Debug.Trace




registerSrc id src = do
    st <- getState
    setState $ ParseState.registerSrc id src st

genID' p = do
    id  <- genID
    tok <- p
    registerSrc id (Token.range tok)
    return (id, Token.value tok)


tokPrep f = do
    id  <- genID
    let range = mempty
    registerSrc id range
    return $ Token (Just id) (f id) range

tok0' f p = do
    tok <- p 
    id  <- case Token.id tok of
                Nothing -> genID
                Just id -> pure id
    registerSrc id (Token.range tok)
    return $ Token (Just id) (f id) (Token.range tok)


tok1 f p = do
    tok <- p 
    id  <- case Token.id tok of
                Nothing -> genID
                Just id -> pure id
    registerSrc id (Token.range tok)
    return $ Token (Just id) (f id $ Token.value tok) (Token.range tok)


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
pPath1     p = sepBy1' p L.pAccessor
pCon         = Token.value <$> L.pIdentType
pCon'        = L.pIdentType
pVar         = do
    x <- L.pIdentVar
    return $ trace(show x) (Token.value x)
pVar'        = L.pIdentVar
pIdent       = choice [ pCon, pVar ]
pIdent'      = choice [ pCon', pVar' ]
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


infixl 4 <$#, <*#>, <*#

l <*# r = do
    Token id value lRange <- l
    Token _ _ rRange             <- r
    let range = mappend lRange rRange
    case id of
        Just v  -> registerSrc v range
        Nothing -> return ()
    return $ Token id value range

l <*#> r = do
    Token lId lValue lRange <- l
    Token rId rValue rRange <- r
    let range = mappend lRange rRange
    case lId of
        Just v  -> registerSrc v range
        Nothing -> return ()
    return $ Token lId (lValue rValue) range

l <$# r = tokPrep l <*# r

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------
--pImport          = (tokPrep Expr.Import    <*+ L.pImport)
--                                          <*>+ (Token.value . Token.flatten <$> pPath1 pIdent')

pImport          = Expr.Import <$#  L.pImport
                               <*#> (Token.flatten <$> pPath1 pIdent')
                               <*#  L.pBlockBegin
                               <*#> (try (Expr.Wildcard <$# L.pImportAll) <|> pIdentE)
                               <*#> pure (Token Nothing Nothing mempty )
                                    -- <*> (     try (Just <$ L.pAs <*> (L.pIdent <?> "import name"))
                                    --       <|> pure Nothing
                                    --     )


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


pClass           = tok Class.mk     <*  L.pClass
                                    <*> (tok Type.Class <*> (pCon                 <?> "class name")
                                                        <*> (many L.pIdentTypeVar <?> "class parameters"))
                                    <??$> pBlockBegin pClassBody
                                    <?> "class definition"


pModule name     = tok Module.mk    <*>   (tok Type.Module <*> pure name)
                                    <??$> withPos (multiBblock pModuleBody)



pClassBody       = choice [ Expr.addMethod <$> pFunc
                          , pCombine Expr.addField pFields
                          , Expr.addClass  <$> pClass
                          ]
                <?> "class body"

pModuleBody      = choice [ Module.addMethod <$> pFunc
                          , pCombine Module.addField pFields
                          , Module.addClass  <$> pClass
                          , Module.addImport <$> (Token.value <$> pImport)
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

pDeclaration   = choice [ -- pImport 
                         pFunc   
                        , pClass  
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


pExpr =   try (tok Expr.Assignment <*> pPattern <* (L.reservedOp "=") <*> pOpE)
      <|> pOpE
      <?> "expression"

pOpE      = Expr.aftermatch <$> PExpr.buildExpressionParser optableE pDotTermE
          
pDotTermE = (pEntBaseE) <??> (flip applyAll <$> many1 (tok Expr.Accessor <* L.pAccessor <*> pVar))

pEntBaseE = choice[ pDeclaration 
                  , try $ L.parensed pExpr
                  --, pIdentE
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



pVarE   = tok1 Expr.Var $ pVar'
pConE   = tok1 Expr.Con $ pCon'

pIdentE = choice [ pVarE
                 , pConE
                 ]

pListExpr = choice [ try $ tok Expr.RangeFromTo <*> pExpr <* L.pRange <*> pExpr
                   , try $ tok Expr.RangeFrom   <*> pExpr <* L.pRange
                   , pExpr 
                   ]

pExprBlock  = pBlockBegin pExpr


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

pBlockBegin     p = L.pBlockBegin *> spaces *> indented *> withPos(multiBblock p)

blockSpaces = try (spaces <* checkIndent) <|> pure ()

multiBblock p = block (p <* blockSpaces)

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

pExprTemp = do
    out <- pExpr <* many(L.eol <* L.pSpaces) <* eof
    id  <- getState
    return (out, id)


pProgram :: [String] -> ParsecT String ParseState.ParseState (State SourcePos) Module.Module
pProgram mod = spaces *> pModule mod <* (spaces <?> "") <* eof

pResult mod = (\ast st -> (ast, view ParseState.sourceMap st)) <$> pProgram mod <*> getState
    --ast <- pProgram
    --st  <- getState
    --return (ast, view ParseState.sourceMap st)

parse (Source.Source mod code) = fst $ flip runState (initialPos "") $ runParserT (pResult mod) def "Luna Parser" code






