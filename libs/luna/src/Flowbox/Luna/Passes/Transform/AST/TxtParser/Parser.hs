---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


tokPure a = pure $ Token Nothing a mempty

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


infixl 4 <$#>, <$#, <*#>, <*#

l <*# r = do
    Token id value lRange <- l
    Token _ _ rRange             <- r
    let range = mappend lRange rRange
    case id of
        Just v  -> registerSrc v range
        Nothing -> return ()
    return $ Token id value range

l *#> r = do
    Token _ _ lRange      <- l
    Token id value rRange <- r
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

l <$#> r = tokPrep l <*#> r

l <$# r = tokPrep l <*# r

p <**#> q = tokPure (\f g -> g f) <*#> p <*#> q
p <??#> q = p <**#> (q <|> tokPure Prelude.id)

--p <??$#> q = p <**#> ((flip (foldr ($)) <$#> q) <|> tokPure Prelude.id)

-----------------------------------------------------------
-- Entities
-----------------------------------------------------------
pTuple     p = L.braced (sepBy p L.separator)
pImplTuple p = sepBy2 p L.separator
pCallList  p = L.parensed (sepBy p L.separator)
pArgListL  p = try(L.parensed (sepBy2 p L.separator)) <|> ((:[]) <$> (try p <|> L.parensed p))
pArgList   p = try(L.parensed (sepBy2 p L.separator)) <|> many (try p <|> L.parensed p)
pArgList2  p = try(L.parensed (sepBy2 p L.separator)) <|> ((:[]) <$> p)
pList      p = L.bracketed (sepBy p L.separator)
pPath1     p = sepBy1_ng p L.pAccessor
pCon         = Token.value <$> L.pIdentType
pCon'        = L.pIdentType
pVar         = do
    x <- L.pIdentVar
    return $ (Token.value x)
pVar'        = L.pIdentVar
pIdent       = choice [ pCon, pVar ]
pIdent'      = choice [ pCon', pVar' ]
pExtPath     = (pPath1 pCon <* L.pAccessor) <|> pure []
wildcard     = storePos L.pWildcard

pExtPath'     = (pPath1' pCon' <*# accessor) <|> tokPure []

pArgList'  p = try(parensed (sepBy2' p L.separator)) <|> many' (try p <|> parensed p)

separator    = storePos L.separator

pAssignment = storePos L.pAssignment

parensed p = between' (storePos L.parenL) (storePos L.parenR) p

braced p = between' (storePos L.braceL) (storePos L.braceR) p

bracketed p = between' (storePos L.bracketL) (storePos L.bracketR) p

tuple  p = braced (Token.flatten <$> sepBy p L.separator)

list   p = bracketed (Token.flatten <$> sepBy p L.separator)

typeDecl = storePos L.pTypeDecl

nativeSym = storePos L.pNativeSym

accessor = storePos L.pAccessor

between' sepL sepR p = sepL *#> p <*# sepR

sepBy'  p sep = Token.flatten <$> sepBy  p sep
sepBy1' p sep = Token.flatten <$> sepBy1 p sep
sepBy2' p sep = Token.flatten <$> sepBy2 p sep
many'   p     = Token.flatten <$> many p
many1'  p     = Token.flatten <$> many1  p
pPath1' p     = Token.flatten <$> pPath1 p

-----------------------------------------------------------
-- Literals
-----------------------------------------------------------

pIntL    = Lit.Integer <$#> storePos L.integerStr
pFloatL  = Lit.Float   <$#> storePos L.floatStr
pCharL   = Lit.Char    <$#> storePos L.charLiteral
pStringL = Lit.String  <$#> storePos L.stringLiteral
pLit     = choice [ try $ pFloatL
                  , pIntL
                  , pCharL 
                  , pStringL
                  ]

-----------------------------------------------------------
-- Declarations
-----------------------------------------------------------

pImport          = Expr.Import <$#  L.pImport
                               <*#> (Token.flatten <$> pPath1 pIdent')
                               <*#  L.pBlockBegin
                               <*#> (try (Expr.Wildcard <$# L.pImportAll) <|> pIdentE)
                               <*#> (     try (tokPure Just <*# L.pAs <*#> (pIdent' <?> "import name"))
                                      <|> tokPure Nothing
                                    )


pArg            = Expr.Arg      <$#> pPatCon
                                <*#> tokPure Nothing -- ((Just <$ pAssignment <*#> pExpr') <|> pure Nothing)

pFunc           = Expr.Function <$#  (storePos L.pDef)
                                <*#> (pExtPath'      <?> "")
                                <*#> (pVar'          <?> "function name")
                                <*#> (pArgList' pArg  <?> "dunction argument list")
                                <*#> (try (storePos L.pArrow *#> pType) <|> tokPrep Type.Unknown)
                                <*#> (pExprBlock' <|> tokPure [])
                                <?> "function definition"


pLambda          = tok Expr.Lambda  <*> pArgListL (Token.value <$> pArg)
                                    <*> (try (L.pArrow *> (Token.value <$> pType)) <|> tok Type.Unknown)
                                    <*> pExprBlock
                                    <?> "lambda definition"


pClass           = tok Class.mk     <*  L.pClass
                                    <*> (tok Type.Class <*> (pCon                 <?> "class name")
                                                        <*> (many L.pIdentTypeVar <?> "class parameters"))
                                    <??$> pBlockBegin pClassBody
                                    <?> "class definition"


pModule name     = tok Module.mk    <*>   (tok Type.Module <*> pure name)
                                    <??$> withPos (multiBblock pModuleBody)



pClassBody       = choice [ Expr.addMethod <$> (Token.value <$> pFunc)
                          , pCombine Expr.addField (Token.value <$> pFields)
                          , Expr.addClass  <$> pClass
                          ]
                <?> "class body"

pModuleBody      = choice [ Module.addMethod <$> (Token.value <$> pFunc)
                          , pCombine Module.addField (Token.value <$> pFields)
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
               <*> (Token.value <$> pType)
               <*> (L.pAssignment *> (Just <$> pExpr) <|> pure Nothing)

mkField t val name id = Expr.Field id name t val

-- FIXME: tok is resolved not by Token
pFields        =  tokPure (\names t val -> map (tok . (mkField t val)) names )
               <*#> (sepBy1' pIdent' L.separator)
               <*#  typeDecl
               <*#> pType
               <*#> (pAssignment *#> (fmap Just <$> pExpr') <|> tokPure Nothing)

pDeclaration   = choice [ Token.value <$> pImport 
                        , Token.value <$> pFunc   
                        , pClass  
                        , try $ pLambda
                        ]


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------


buildExpressionParser operators simpleExpr
    = foldl (makeParser) simpleExpr operators
    where
      makeParser term lassoc
        = let 
              lassocOp   = choice lassoc

              lassocP (Token _ x rangeL)  = do 
                  Token id f rangeM <- lassocOp
                  Token _ y rangeR <- term
                  let range = mappend rangeL rangeR
                  case id of
                      Just v  -> registerSrc v range
                      Nothing -> return ()
                  lassocP1 (Token id (f x y) range)
                             
              lassocP1 x = lassocP x <|> return x

           in  do x <- term
                  lassocP x <|> return x
                 

pNative     = between nativeSym nativeSym (many' pNativeElem)
pNativeElem = choice [ pNativeVar
                     , pNativeCode
                     ]
pNativeVar  = Expr.NativeCode <$#> many1' (storePos $ noneOf "`#")
pNativeCode = Expr.NativeVar  <$#> storePos (L.symbols "#{" *> many (noneOf "}") <* L.symbol '}')

pExpr = Token.value <$> pExpr'

pExpr' =  try (Expr.Assignment <$#> pPattern <*# (storePos $ L.reservedOp "=") <*#> pOpE)
      <|> pOpE
      <?> "expression"

pOpE      = fmap Expr.aftermatch <$> buildExpressionParser optableE'' pDotTermE
          
pDotTermE = (pEntBaseE) <??#> (tokPure (flip applyAll) <*#> many1' (Expr.Accessor <$# accessor <*#> pVar'))

pEntBaseE = choice[ tokPure =<< pDeclaration 
                  , try $ parensed pExpr'
                  , pIdentE
                  , Expr.Lit    <$#> pLit
                  , Expr.Tuple  <$#> tuple pExpr'
                  , Expr.List   <$#> list  pListExpr
                  , Expr.Native <$#> pNative
                  ]
           <?> "expression term"

binaryM'  name fun = (L.reservedOp name *>        fun)

binaryM''  name fun = (storePos (L.reservedOp name) *#>        fun)

optableE'' = [ [ binaryM''  ""   (tokPrep Expr.callConstructor)            ]
             , [ operator "^"                                              ]
             , [ operator "*"                                              ]
             , [ operator "/"                                              ]
             , [ operator "+"                                              ]
             , [ operator "-"                                              ]
             , [ binaryM''  "$"  (tokPrep $ binaryMatchE' . Expr.callConstructor) ]
             ]
             where
                operator op = binaryM'' op ((binaryMatchE' . Expr.Infix <$#> tokPure ('~':op)))
                --operator op = binaryM'' op (tokPure binaryMatchE' <*#> (Expr.Infix <$#> tokPure ('~':op)))

--optableE' = [ [ binaryM'  ""   (tok Expr.callConstructor)                  ]
--            , [ operator "^"                                               ]
--            , [ operator "*"                                               ]
--            , [ operator "/"                                               ]
--            , [ operator "+"                                               ]
--            , [ operator "-"                                               ]
--            , [ binaryM'  "$"  (binaryMatchE <$> tok Expr.callConstructor) ]
--            ]
--            where
--               operator op = binaryM' op (binaryMatchE <$> (tok Expr.Infix <*> pure ('~':op)))

--optableE = [ [ postfixM "::" (tok Expr.Typed <*> (Token.value <$> pType))                      ]
--           , [ binaryM  ""   (tok Expr.callConstructor)      PExpr.AssocLeft ]
--           , [ operator "^"                                  PExpr.AssocLeft ]
--           , [ operator "*"                                  PExpr.AssocLeft ]
--           , [ operator "/"                                  PExpr.AssocLeft ]
--           , [ operator "+"                                  PExpr.AssocLeft ]
--           , [ operator "-"                                  PExpr.AssocLeft ]
--           , [ binaryM  "$"  (binaryMatchE <$> tok Expr.callConstructor)      PExpr.AssocLeft ]
--           ]
--           where
--              operator op = binaryM op (binaryMatchE <$> (tok Expr.Infix <*> pure ('~':op)))
binaryMatchE' f p q = f p q

binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)



pVarE   = Expr.Var <$#> pVar'
pConE   = Expr.Con <$#> pCon'

pIdentE = choice [ pVarE
                 , pConE
                 ]

pListExpr = choice [ try $ Expr.RangeFromTo <$#> pExpr' <*# (storePos L.pRange) <*#> pExpr'
                   , try $ Expr.RangeFrom   <$#> pExpr' <*# (storePos L.pRange)
                   , pExpr'
                   ]

pExprBlock  = pBlockBegin pExpr

pExprBlock' = Token.flatten <$> pBlockBegin pExpr'



-----------------------------------------------------------
-- Types
-----------------------------------------------------------
pType       = choice [ --try $ pLambdaT
                      try $ pConAppT
                     , pTermT
                     ]
              <?> "type"

pTermT      = choice[ try $ parensed pType
                    , pEntT
                    ]
              <?> "type term"

pConAppT    = Type.App     <$#> pAppBaseT <*#> many1' pTermT
--pLambdaT    = tok Type.Lambda  <*> pArgList2 pTermT <* L.pArrow <*> pTermT
pVarT       = Type.Var     <$#> pVar'
pConT       = Type.Con     <$#> pPath1' pCon'
pTupleT     = Type.Tuple   <$#> tuple pType
pWildcardT  = Type.Unknown <$#  wildcard
----pLambdaT    i   = Type.Lambda <$> pTupleT i <*> return Type.Unknown

pAppBaseT   = choice [ pVarT  
                     , pConT  
                     ]

pEntT       = choice [ pAppBaseT 
                     , pTupleT
                     , pWildcardT
                     ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pPattern    = choice [ try $ (Pat.Tuple <$#> sepBy2' pPatCon separator)
                     , pPatCon 
                     ]

pPatCon     = choice [ pConAppP
                     , pTermP
                     ]

pTermP      = choice [ try $ parensed pPatCon
                     , try (Pat.Typed <$#> pEntP <*# typeDecl <*#> pType)
                     , pEntP
                     ]
              <?> "pattern term"

pVarP       = Pat.Var      <$#> pVar'
pLitP       = Pat.Lit      <$#> pLit
pTupleP     = Pat.Tuple    <$#> tuple pPatCon
pWildcardP  = Pat.Wildcard <$#  wildcard
pConP       = Pat.Con      <$#> pCon'
pConAppP    = Pat.App      <$#> pConP <*#> many1' pTermP

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

--pExprTemp = do
--    out <- pExpr <* many(L.eol <* L.pSpaces) <* eof
--    id  <- getState
--    return (out, id)


pProgram :: [String] -> ParsecT String ParseState.ParseState (State SourcePos) Module.Module
pProgram mod = spaces *> pModule mod <* (spaces <?> "") <* eof

pResult mod = (\ast st -> (ast, view ParseState.sourceMap st)) <$> pProgram mod <*> getState
    --ast <- pProgram
    --st  <- getState
    --return (ast, view ParseState.sourceMap st)

parse (Source.Source mod code) = fst $ flip runState (initialPos "") $ runParserT (pResult mod) def "Luna Parser" code






