---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}


--{-# LANGUAGE OverlappingInstances #-}

module Luna.Parser.Parser where


import           Control.Applicative
import           Control.Exception            (bracket)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Data.ByteString              as B
import qualified Data.ByteString.UTF8         as UTF8
import           Data.CharSet.ByteSet         as S
import           Data.Default
import           Flowbox.Prelude              hiding (noneOf, maybe, element, cons)
import qualified Flowbox.Prelude              as Prelude
import qualified Luna.Data.ASTInfo            as ASTInfo
import qualified Luna.Parser.Token            as Tok
import qualified Luna.Parser.State            as State
import           Luna.Parser.State            (State)
import           System.Environment           (getArgs)
import           System.IO                    (IOMode (ReadMode), hClose, openFile)
import           System.IO                    (stdout)
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import           Text.RawString.QQ
import           Text.Trifecta                hiding (parseFromFile, parseByteString, parseString)
import qualified Text.Trifecta                as Trifecta
import           Text.Trifecta.Delta          as Delta
import qualified Luna.Data.Config             as Config
import qualified Luna.Pragma.Pragma           as Pragma
import           Luna.Pragma.Pragma           (IsPragma)
import           Data.Typeable
import           Data.String.Utils            (join)
import           Luna.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Data.Char                    (isSpace)
import qualified Data.ByteString as ByteStr
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))

import qualified Luna.Data.Namespace          as Namespace
import qualified Luna.Data.AliasInfo          as Alias
import qualified Luna.ASTNew.AST              as AST
import qualified Luna.ASTNew.Traversals       as AST
import qualified Data.Maps                    as Map
import           Data.Maybe                   (fromJust)
import qualified Luna.AST.Arg                 as Arg
import qualified Data.List                    as List
import qualified Luna.Parser.Pragma           as Pragma

import           Text.EditDistance            --(defaultEditCosts, levenshteinDistance, EditCosts, Costs(..))
import           Text.PhoneticCode.Phonix     (phonix)
import           Data.Function                (on)

import qualified Data.IntMap  as IntMap

--import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import qualified Text.Parsers.Indent as Indent


import qualified Luna.ASTNew.Expr as Expr


import qualified Luna.ASTNew.Decl   as Decl
import           Luna.ASTNew.Decl   (Field(Field))
import qualified Luna.ASTNew.Module as Module
import           Luna.ASTNew.Module (Module(Module), LModule)
import           Luna.ASTNew.Unit   (Unit(Unit))
import qualified Luna.ASTNew.Label  as Label
import           Luna.ASTNew.Label  (Label(Label))
import qualified Luna.ASTNew.Type   as Type
import           Luna.ASTNew.Type   (Type)
import qualified Luna.ASTNew.Pat    as Pat
import           Luna.ASTNew.Pat    (Pat)
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native

import qualified Luna.ASTNew.Enum       as Enum
import           Luna.ASTNew.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.ASTNew.Unit       as Unit


infixl 4 <$!>


mtry p = try p <|> pure mempty

vName = Name.V <$> varOp
tName = Name.T <$> Tok.typeIdent

anyName = vName <|> tName

just p = Just <$> p

labeled p = do
    id <- nextID
    fmap (label id) p

label id = Label $ IDTag id


(<$!>) :: Monad m => (a -> b) -> m a -> m b
f <$!> ma = do
  a <- ma
  return $! f a

mytoken :: CharParsing m => m Char
mytoken = noneOf $ ['\0'..'\31'] ++ "()<>@,;:\\\"/[]?={} \t" ++ ['\128'..'\255']

isHSpace :: Char -> Bool
isHSpace c = c == ' ' || c == '\t'

skipHSpaces :: CharParsing m => m ()
skipHSpaces = skipSome (satisfy isHSpace)

data Request = Request {
      requestMethod   :: String
    , requestUri      :: String
    , requestProtocol :: String
    } deriving (Eq, Ord, Show)

requestLine :: (Monad m, TokenParsing m) => m Request
requestLine = Request <$!> (highlight ReservedIdentifier (some mytoken) <?> "request method")
                       <*  skipHSpaces
                       <*> (highlight Identifier (some (satisfy (not . isHSpace))) <?> "url")
                       <*  skipHSpaces
                       <*> (try (highlight ReservedIdentifier (string "HTTP/" *> many httpVersion <* endOfLine)) <?> "protocol")
  where
    httpVersion :: (Monad m, CharParsing m) => m Char
    httpVersion = satisfy $ \c -> c == '1' || c == '0' || c == '.' || c == '9'

endOfLine :: CharParsing m => m ()
endOfLine = (string "\r\n" *> pure ()) <|> (char '\n' *> pure ())

data Header = Header {
      headerName  :: String
    , headerValue :: [String]
    } deriving (Eq, Ord, Show)

messageHeader :: (Monad m, TokenParsing m) => m Header
messageHeader = (\h b c -> Header h (b : c))
            <$!> (highlight ReservedIdentifier (some mytoken)  <?> "header name")
             <*  highlight Operator (char ':') <* skipHSpaces
             <*> (highlight Identifier (manyTill anyChar endOfLine) <?> "header value")
             <*> (many (skipHSpaces *> manyTill anyChar endOfLine) <?> "blank line")

request :: (Monad m, TokenParsing m) => m (Request, [Header])
request = (,) <$> requestLine <*> many messageHeader <* endOfLine




tuple         p = Tok.parens (sepBy p Tok.separator)
qualifiedPath p = sepBy1_ng p Tok.accessor <?> "qualified path"
extensionPath   = (,) <$> (((qualifiedPath Tok.typeIdent <?> "extension path") <* Tok.accessor) <|> pure [])
                      <*> (namePattern <?> "function name")

namePattern =   (MultiName.single <$> varOp)
            <|> Tok.parens (MultiName.close <$> (MultiName.multi <$> Tok.varIdent <*> many1 namePatSeg))

namePatSeg =   (MultiName.Token <$> Tok.varIdent)
           <|> (MultiName.Hole  <$  Tok.nameWildcard)

argList       p = try (sepBy2 p Tok.separator) <|> many p <?> "argument list"
argList'      p = braces (sepBy2 p Tok.separator) <|> ((:[]) <$> p) <?> "argument list"
list          p = Tok.brackets (sepBy p Tok.separator)
anyIdent        = choice [ Tok.varIdent, Tok.typeIdent ]

varOp           = Tok.varIdent <|> Tok.operator



getASTInfo = view State.info <$> get

putASTInfo info = modify (State.info .~ info)

nextID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> nextID


binary   name fun assoc = Infix   (Tok.reservedOp name *> return fun) assoc
binaryM  name fun assoc = Infix   (Tok.reservedOp name *>        fun) assoc
prefix   name fun       = Prefix  (Tok.reservedOp name *> return fun)
prefixM  name fun       = Prefix  (Tok.reservedOp name *>        fun)
prefixfM      fun       = Prefix  (fun)
postfix  name fun       = Postfix (Tok.reservedOp name *> return fun)
postfixM name fun       = Postfix (Tok.reservedOp name *>        fun)


element m = do
    id <- nextID
    State.registerID id
    ast <- m id
    --State.registerAST id ast
    return ast



container m = element $ \id -> State.withScope id $ m id




--regVarName m id = do
--    ast <- m id
--    State.regVarName id (AST.name ast)
--    return ast


--nameTok p = element $ regVarName $ \id -> p <*> pure id

tok p = element $ \id -> p <*> pure id

-- Parser translation unit.
-- Provides a global namespace when parsing module, expression etc.
unit p = do
    --FIXME[WD] : change id to datatype
    let id = -666 
    --id <- nextID
    --Unit id <$> State.withScope id p
    State.withScope id p


-----------------------------------------------------------
-- Definitions
-----------------------------------------------------------


pUnit p = Unit <$> labeled p


decl = choice [ imp, func, cls, typeAlias, typeWrapper ]

----- Modules -----

pModule name path = Module <$> pure path 
                           <*> pure name 
                           <*> Indent.withPos (moduleBlock $ labeled moduleBody)
                    where moduleBody = decl <?> "module body"


----- Imports -----

imp = Decl.Import <$  Tok.kwImport
                  <*> (qualifiedPath Tok.typeIdent <?> "import path")
                  <*> ((Just <$ Tok.kwAs <*> Tok.typeIdent) <|> pure Nothing)
                  <*> (blockBegin importTarget <|> pure [])
                  <?> "import declaration"

importTarget =   body Decl.ImpVar varOp 
             <|> body Decl.ImpType Tok.typeIdent
             where body c p = c <$> p <*> ((Just <$ Tok.kwAs <*> p) <|> pure Nothing)


----- type aliases ------

typeAlias = Decl.TypeAlias <$  Tok.kwAlias 
                           <*> (typeT <?> "new type") 
                           <*  Tok.assignment 
                           <*> (typeT <?> "base type")
                           <?> "type alias"


----- type wrappers ------

typeWrapper = Decl.TypeWrapper <$  Tok.kwType 
                               <*> (typeT <?> "new type") 
                               <*  Tok.assignment 
                               <*> (typeT <?> "base type")
                               <?> "type wrapper"


----- functions -----

func = Decl.Function <$  Tok.kwDef
                     <*> extPath
                     <*> name
                     <*> (argList arg <?> "function argument list")
                     <*> outType
                     <*> body
    where extPath = ((qualifiedPath Tok.typeIdent <?> "extension path") <* Tok.accessor) <|> pure []
          name    = namePattern <?> "function name"
          outType = (Just <$> try (Tok.arrow *> typeT)) <|> pure Nothing
          body    = char ':' *> stage1Body2


----- classes -----

cls = do
    name <- Tok.kwClass *> (Tok.typeIdent <?> "class name")
    Decl.Data <$> pure name
              <*> params
              <*  blockStart
              <*> constructors name
              <*> bodyBlock
              <*  blockEnd
              <?> "class definition"
      where params         = many (TVName <$> Tok.typeVarIdent <?> "class parameter")
            defCons      n = Decl.Cons n <$> (concat <$> many fields)
            constructors n =   blockBody' (labeled cons) 
                           <|> ((:[]) <$> labeled (defCons $ Name.convert n))
            bodyBlock      = blockBodyOpt $ labeled clsBody 
            clsBody        = choice [ func, cls, typeAlias, typeWrapper ] <?> "class body"


cons         = Decl.Cons <$> Tok.conIdent 
                         <*> (concat <$> blockBeginFields fields)
                         <?> "data constructor definition"


fields = do
         (names, cls) <- try ((,) <$> fieldList      <*> typed)
                         <|> ((,) <$> pure [Nothing] <*> termT)
         
         sequence $ fmap (labeled.pure) 
                  $ zipWith3 Field (repeat cls) names (repeat Nothing)

         where fieldList = sepBy1 (Just <$> Tok.varIdent) Tok.separator




typed = Tok.typeDecl *> termT


arg            = Arg <$> argPattern
                     <*> ((Just <$ Tok.assignment <*> stage1DefArg) <|> pure Nothing)

stage1DefArg = Tok.tokenBlock (many alphaNum)

stage1BodyInner = (many1 $ noneOf "\n\r")
stage1Body = (:) <$> stage1BodyInner <*> (try (spaces *> Indent.checkIndented *> stage1Body) <|> pure [[]])

stage1Body2 = ((:) <$> (try ((++) <$> Tok.spaces <* Indent.checkIndented <*> stage1BodyInner)) <*> stage1Body2) <|> pure [[]]

--stage1Block = (++) <$> Tok.spaces <* Ident.checkIndented <*> Indent.withPos (indBlockBody p)
--stage1Body2 = (:) <$> (try (Tok.spaces <* Indent.checkIndented <* stage1BodyInner) <|> pure []) <*> stage1Body2
--dokonczyc bo nie ma wciec

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

typeT       = choice [ try funcT
                     , typeSingle
                     ] <?> "type"

typeSingle  = choice [ try appT
                     , termT 
                     ] <?> "type"

termT       = choice [ try $ Tok.parens typeT 
                     , entT 
                     ] <?> "type term"

appT        = labeled (Type.App <$> appBaseT <*> many1 termT)


argListT    = braces (sepBy2 typeT Tok.separator) <|> ((:[]) <$> typeSingle) <?> "type argument list"
funcT       = labeled (Type.Function <$> argListT <* Tok.arrow <*> typeT)

varT        = labeled (Type.Var      <$> Tok.typeVarIdent)
conT        = labeled (Type.Con      <$> qualifiedPath Tok.typeIdent)
tupleT      = labeled (Type.Tuple    <$> tuple typeT)
listT       = labeled (Type.List     <$> Tok.brackets typeT)
wildT       = labeled (Type.Wildcard <$  Tok.wildcard)

appBaseT    = choice [ varT, conT
                     ]

entT        = choice [ varT
                     , conT
                     , tupleT
                     , listT
                     , wildT
                     ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pattern    = choice [ try implTupleP
                    , patCon
                    ]

patTup     = pattern <|> (labeled (Pat.Tuple <$> pure []))

patCon     = choice [ try appP
                    , termP
                    ]

argPattern = termBase termT

termP      = termBase typeT

termBase t = choice [ try (labeled (Pat.Grouped <$> Tok.parens patTup))
                    , try (labeled (Pat.Typed   <$> entP <* Tok.typeDecl <*> t))
                    , entP
                    ]
              <?> "pattern term"

varP       = labeled (Pat.Var         <$> Tok.varIdent)
litP       = labeled (Pat.Lit         <$> literal)
implTupleP = labeled (Pat.Tuple       <$> sepBy2 patCon Tok.separator)
wildP      = labeled (Pat.Wildcard    <$  Tok.wildcard)
recWildP   = labeled (Pat.RecWildcard <$  Tok.recWildcard)
conP       = labeled (Pat.Con         <$> Tok.conIdent)
appP       = labeled (Pat.App         <$> conP <*> many1 termP)

entP = choice [ varP
              , litP
              , wildP
              , recWildP
              , conP
              ]


----------------------------------------------------------------------
-- Literals
----------------------------------------------------------------------

literal = choice [ numL, charL, stringL ]
charL   = labeled (Lit.Char   <$> Tok.charLiteral)
stringL = labeled (Lit.String <$> Tok.stringLiteral)
numL    = labeled (Lit.Number <$> Tok.numberL)


prevParsedChar = do
    Caret delta bs <- careting
    --return ' '
    return $ (UTF8.toString bs !! (max 0 . fromIntegral $ column delta - 1))

lastLexemeEmpty = do
    prevChar <- prevParsedChar
    when (isSpace prevChar) $ fail "not empty"


-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
expr       = exprT entBaseE

exprSimple = exprT pEntBaseSimpleE

exprT base =   try (labeled (Expr.Assignment <$> pattern <* (Tok.reservedOp "=") <*> opTupleTE base)) -- FIXME
           <|> opTupleTE base
           <?> "expression"


opE       = opTE entBaseE
opTupleTE base = tupleE $ opTE base

opTE base = buildExpressionParser optableE (appE base)

tupleE p = p <??> ((\id xs x -> label id $ Expr.Tuple (x:xs)) <$> nextID <* Tok.separator <*> sepBy1 p Tok.separator)

--appE base = p <??> (appID (\i a s -> Expr.App i s a) <*> many1 (argE p)) where 
appE base = p <??> ((\i a s -> label i $ callBuilder2 s a) <$> nextID <*> many1 (argE p)) where 
    p = termE base



argE p = try ((Expr.Named <$> Tok.varIdent <* Tok.assignment <*> p)) <|> ((Expr.Unnamed <$> p))

termE base = base <??> (flip applyAll <$> many1 (termBaseE base))  ------  many1 (try $ recUpdE))


termBaseE p = choice [ try recUpdE
                     , accE
                     , callTermE p
                     ]

recAcc  = (Tok.accessor *> varOp)

accBaseE  = (Tok.accessor *> nameBase)

nameBase =   (Name.VarName  <$> varOp)
         <|> (Name.TypeName <$> Tok.conIdent)


recUpdE   = (\id sel expr src -> label id (Expr.RecUpdt src sel expr)) <$> nextID <*> many1 recAcc <* Tok.assignment <*> exprSimple

accE      = try( (\id a b -> label id $ Expr.Accessor a b) <$> nextID <*> accBaseE) -- needed by the syntax [1..10]







parensE p = Tok.parens (p <|> (labeled (Expr.Tuple <$> pure []))) -- checks for empty tuple

callList      p = Expr.Seq <$> Tok.parens (sepBy p Tok.separator)
callTermE p = (\id a b-> label id (Expr.App b a)) <$ lastLexemeEmpty <*> nextID <*> callList (argE p)


entBaseE        = entConsE entComplexE
pEntBaseSimpleE = entConsE entSimpleE

entConsE base = choice [ try $ labeled (Expr.Grouped <$> parensE (exprT base))
                       , base
                       ]

entComplexE = choice[ --labeled (Expr.Decl <$> labeled decl) -- FIXME: zrobic subparsowanie!
                    entSimpleE
                    ]
             <?> "expression term"

entSimpleE = choice[ caseE -- CHECK [wd]: removed try
                   --, condE
                   , labeled $ Expr.Grouped <$> parensE expr
                   , identE
                   --, try (labeled Expr.RefType <$  Tok.ref <*> Tok.conIdent) <* Tok.accessor <*> varOp
                   , labeled $ Expr.Ref     <$  Tok.ref <*> entSimpleE
                   , labeled $ Expr.Lit     <$> literal
                   , labeled $ listE
                   --, labeled $ Expr.Native  <$> nativeE
                   ]
           <?> "expression term"

optableE = [ 
           --, [ prefixM   "@"  (appID Expr.Ref)                                  ]
             --[ binaryM   ""   (callBuilder <$> genID <*> genID)    AssocLeft ]
             [ operator4 "^"                                  AssocLeft ]
           , [ operator4 "*"                                  AssocLeft ]
           , [ operator4 "/"                                  AssocLeft ]
           , [ operator4 "+"                                  AssocLeft ]
           , [ operator4 "-"                                  AssocLeft ]
           , [ operator4 "<"                                  AssocLeft ]
           , [ operator4 ">"                                  AssocLeft ]
           , [ operator4 "=="                                 AssocLeft ]
           , [ operator4 "in"                                 AssocLeft ]
           , [ binaryM   "$"  (callBuilder <$> nextID)        AssocLeft ]
           , [ postfixM  "::" ((\id a b -> label id (Expr.Typed a b)) <$> nextID <*> typeT) ]
           ]
           where
              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
              operator4 op = binaryM op ( (\id1 id2 l r -> label id1 $ Expr.App (label id2 $ Expr.Var op) 
                                                                                (Expr.Infix l r)
                                          ) <$> nextID <*> nextID)

              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure ('~':op)))
              --operator2 op = binaryM op (binaryMatchE <$>  ( appID Expr.App <*> (appID Expr.Accessor <*> pure "add" <*> ... ) )  )
              --operator2 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID)
              --operator3 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 "contains" y) [x]) <$> genID <*> genID)

callBuilder id src@(Label lab expr) arg = label id $ case expr of
    Expr.App src' (Expr.Seq args) -> Expr.App src' (Expr.Seq $ args ++ [Expr.Unnamed arg])
    _                             -> Expr.App src (Expr.Seq $ [Expr.Unnamed arg])

callBuilder2 src@(Label lab expr) argsx = case expr of
    Expr.App src' (Expr.Seq args) -> Expr.App src' (Expr.Seq $ args ++ argsx)
    _                             -> Expr.App src  (Expr.Seq argsx)


--callBuilder id id2 src arg = case arg of
--    Expr.App id' src' args -> Expr.App id' src (Arg.Named id2 "X!" src' : args)
--    _                      -> Expr.App id src [Arg.Named id2 "Y" arg]

--binaryM2  name fun assoc = PExpr.Infix   (L.reserved name *>        fun) assoc

--binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)

--withReservedWords words p = do
--    mapStateVal $ State.addReserved words
--    ret <- p
--    mapStateVal $ State.delReserved words
--    return ret



--mkFuncParser func defparser = case name of
--    (Name base segments) -> multiparser
--    _                          -> defparser
--    where name          = Expr._fname func
--          argExpr       = argE expr
--          exprApp p a b = (:) <$> p <* a <*> b
--          segParsers    = fmap (Tok.symbol) segments
--          argParser     = foldr (exprApp argExpr) ((:[]) <$> argExpr) segParsers
--          (Name base segments) = name
--          multiparser   = withReservedWords segments $ tok (Expr.app <$> tok (pure $ Expr.var fname) <*> argParser)
--          [s1,s2] = fmap Tok.symbol segments
--          fname = if null segments then base 
--                                   else base ++ " " ++ join " " segments

--mkFuncParser func = State.withReserved (segNames segments) $ tok (Expr.app <$> tok (pure $ Expr.funcVar name) <*> argParser)
--mkFuncParser func = State.withReserved (segNames segments) $ tok (Expr.App <$> tok (pure $ Expr.funcVar name) <*> argParser)
--    where name          = Expr._fname func
--          argExpr       = argE expr
--          exprApp a b   = (++) <$> a <*> b
--          segParsers    = fmap segParser segments
--          argParser     = foldr exprApp (pure []) segParsers
--          (MultiName base segments) = name

--          segParser seg = case seg of
--              MultiName.Hole    -> (:[]) <$> argExpr
--              MultiName.Token s -> []    <$  Tok.symbol s

--          segNames = segNames' []
--          segNames' names s = case s of
--              []   -> names
--              x:xs -> case x of
--                  MultiName.Token n -> segNames' (n:names) xs
--                  MultiName.Hole    -> segNames' names     xs

notReserved p = do
    rsv  <- view State.adhocReserved <$> get
    name <- p
    if name `elem` rsv then fail $ "'" ++ name ++ "' is a reserved word"
                       else return name


---
varE   = do
    name <- try $ notReserved Tok.varIdent
    ast  <- lookupAST name
    case ast of
        -- FIXME[wd]: dopiero przy dwuprzebiegowym parserze bedziemy mieli wieloczlonowe funkcje rekurencyjne
        Just(AST.Decl func@(Decl.Function {})) -> labeled . pure $ Expr.Var name -- mkFuncParser func -- FIXME - function jest deklaracją!
        _                                      -> labeled . pure $ Expr.Var name
                          

lookupAST name = do
    scope  <- State.getScope
    astMap <- State.getASTMap
    pid    <- State.getPid

    pragmaSet <- view (State.conf . Config.pragmaSet) <$> get
        
    case Map.lookup pid scope of
            Nothing                    -> fail "Internal parser error [1]"
            Just (Alias.Scope varnames typenames) -> case Map.lookup name varnames of
                -- FIXME[wd]: zwracamy maybe. Nothing zostanie zwrocone przy rekurencji. Poprawic przy dwuprzebiegowym parserze
                -- poprawka: Nothing zostanie rowniez zwrocone przy ustawionej fladze
                -- poprawka: Nothing zostanie rowniez zwrocone przy "self"
                Just dstID -> return $ Map.lookup dstID astMap 
                Nothing    -> if (name == "self") 
                    then return Nothing
                    else case Pragma.lookup pragmaSet of
                        Pragma.Defined Pragma.AllowOrphans -> return Nothing
                        _                                  -> fail $ "name '" ++ name ++ "' is not defined" ++ msgTip
                        where scopedNames = Map.keys varnames
                              simWords    = findSimWords name scopedNames
                              msgTip = if length simWords > 0 then ", perhaps you ment one of {" ++ join ", " (fmap show simWords) ++ "}"
                                                              else ""
                          

editCosts = EditCosts { deletionCosts      = ConstantCost 10
                      , insertionCosts     = ConstantCost 10
                      , substitutionCosts  = ConstantCost 10
                      , transpositionCosts = ConstantCost 10
                      }

editCosts2 = EditCosts { deletionCosts     = ConstantCost 10
                      , insertionCosts     = ConstantCost 1
                      , substitutionCosts  = ConstantCost 10
                      , transpositionCosts = ConstantCost 3
                      }

findSimWords word words = fmap snd simPairs
    --where dist a b = levenshteinDistance editCosts (phonix a) (phonix b)
    where dist a b = levenshteinDistance editCosts2 a b
          simWords = fmap (dist word) words
          simPairs = filter ((<20).fst) 
                   $ List.sortBy (compare `on` fst) 
                   $ zip simWords words


    
--varE   = appID $ Expr.var <*> Tok.varIdent
varOpE = labeled $ Expr.Var  <$> try (Tok.parens varOp)
conE   = labeled $ Expr.Cons <$> Tok.conIdent

identE = choice [ varE
                , varOpE
                , conE
                ]

---


--listE = choice [ try $ appID Expr.RangeFromTo <*> opE <* Tok.range <*> opE
--               , try $ appID Expr.RangeFrom   <*> opE <* Tok.range
--               , opE
--               ]


listE = Expr.List <$> labeled (Tok.brackets listTypes)

listTypes = choice [ try $ Expr.RangeList <$> rangeList opE
                   ,       Expr.SeqList   <$> sepBy opE Tok.separator
                   ]

rangeList p =   (Expr.Geometric <$> p <* Tok.separator <*> p <*> endLimit)
            <|> (Expr.Linear    <$> p <*> endLimit)
            where endLimit = try (Tok.range *> just p) <|> pure Nothing

caseE     = labeled (Expr.Case <$ Tok.kwCase <*> exprSimple <*> (blockBegin caseBodyE <|> return []))
caseBodyE = labeled (Expr.Match <$> pattern <*> exprBlock)


--condE     = appID Expr.Cond <* Tok.kwIf <*> exprSimple <*> exprBlock <*> maybe (indBlockSpacesIE *> Tok.kwElse *> exprBlock)


            --nativeE     = Tok.betweenNative (many nativeElemE)
            --nativeElemE = choice [ nativeVarE
            --                     , nativeCodeE
            --                     ]
            --nativeCodeE = appID Expr.NativeCode <*> ((:) <$> (noneOf "`#") <*> nativeCodeBodyE)
            --nativeVarE  = appID Expr.NativeVar  <*  symbol "#{" <*> many (noneOf "}") <* symbolic '}'

            --nativeCodeBodyE = (try(lookAhead $ string "#{")  *> pure [])
            --              <|> (try(lookAhead $ string "```") *> pure [])
            --              <|> ((++) <$> ((:) <$> anyChar <*> many (noneOf "`#")) <*> nativeCodeBodyE)


exprBlock  = blockBegin expr


----------------------------------------------------------------------
-- Code segments
----------------------------------------------------------------------
--indBlockBody   p = many1 (Indent.checkIndentedOrEq *> p <* indBlockSpaces)

--indBlockSpaces   = try (Tok.spaces <* Indent.checkIndent) <|> pure mempty

moduleBlock p = braceBlockBegin p <|> indBlockBody p
blockBegin  p = indBlockBegin   p <|> braceBlockBegin p

indBlockBegin  p = Tok.indBlockBegin *> indBlock p
indBlock       p = Tok.spaces *> Indent.indented *> Indent.withPos (indBlockBody p)
indBlockBody   p = (:) <$> p <*> many (indBlockPrefix p)
indBlockBody'  p = (:) <$> p <*> many (try $ indBlockPrefix p)
indBlockBodyOpt p = ($) <$> (((:) <$> p) <|> pure id) 
                        <*> many (indBlockPrefix p)
indBlockPrefix p = try ((try (Tok.spaces *> Indent.checkIndent) <|> try (Tok.terminator *> Tok.spaces *> Indent.checkIndentedOrEq )) *> notFollowedBy eof) *> p
--indBlockBody   p = (:) <$> p <*> many (try (Tok.spaces *> Indent.checkIndent) *> p)
indBlockSpacesIE = try (Tok.spaces <* Indent.checkIndentedOrEq) <|> pure mempty

braceBlockBegin p = Tok.braceL *> Tok.spaces *> Indent.withDiscarded (many1 (p <* braceBlockSpaces)) <* Tok.braceR
braceBlockSpaces  = Tok.terminator *> Tok.spaces

codeBlock    p = indCodeBlock p -- <|> braceCodeBlock p
indCodeBlock p = indCodeBlockStart *> indBlockBody p <* indBlockEnd

indCodeBlockStart = Tok.indBlockBegin *> indBlockStart
indBlockStart     = Tok.spaces *> Indent.indented *> Indent.startBlock
indBlockEnd       = Indent.endBlock

blockStart  = indCodeBlockStart
blockEnd    = indBlockEnd
blockBody p = indBlockBody p
blockBody' p = indBlockBody' p
blockBodyOpt p = indBlockBodyOpt p


blockBeginFields p = Tok.indBlockBegin *> indBlockFields p
indBlockFields   p = Tok.spaces *> Indent.indented *> Indent.withPos (indBlockBodyFields p)
indBlockBodyFields   p = (:) <$> p <*> many (indBlockPrefixFields p)
indBlockPrefixFields p = ((try (Tok.spaces *> Indent.checkIndentedOrEq)) *> notFollowedBy eof) *> p


-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

parserName = "Luna Compiler"

run p st = evalStateT (Indent.parser p) st

handleResult r = case r of
    Failure e -> Left e
    Success a -> Right a

bundleResult p = (,) <$> p <*> get

end = (Tok.spaces <?> "") <* (eof <?> "")

upToEnd p = Tok.spaces *> p <* end

renderErr e = renderPretty 0.8 80 $ e <> linebreak

-----------------------------------------------------------
-- Pragmas
-----------------------------------------------------------

appConf = Config.registerPragma (undefined :: Pragma.TabLength)
        . Config.registerPragma (undefined :: Pragma.AllowOrphans)
        . Config.registerPragma (undefined :: Pragma.ImplicitSelf)

-- FIXME[wd]: logika powina byc przeniesiona na system pluginow
defConfig = appConf def
-- FIXME[wd]: debugowo ustawione wartosci typow
defState  = (def :: State () () String ()) & State.conf .~ defConfig


appSt = State.conf %~ appConf

--st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")

parseGen p st = run (bundleResult (unit p)) st

--moduleParser modPath = parseGen (upToEnd $ func)
moduleParser modPath = parseGen (upToEnd $ pUnit $ pModule (last modPath) (init modPath))
--exprParser           = parseGen (upToEnd expr)
exprBlockParser      = parseGen (upToEnd $ indBlock expr)
--patternParser        = parseGen (upToEnd pattern)
--typeParser           = parseGen (upToEnd typeT)

-----------------------------------------------------------
-- Input utils
-----------------------------------------------------------

parserDelta name = Directed (UTF8.fromString name) 0 0 0 0

parseFromByteString = Trifecta.parseByteString

parseFromString p delta input = parseFromByteString p delta (UTF8.fromString input)

parseFromFile p delta path = do
  s <- liftIO $ ByteStr.readFile path
  return $ parseFromByteString p delta s

parseFile       path  p = handleResult <$> parseFromFile       p (parserDelta parserName) path
parseString     input p = handleResult  $  parseFromString     p (parserDelta parserName) input
parseByteString input p = handleResult  $  parseFromByteString p (parserDelta parserName) input


data AliasAnalysis = AliasAnalysis

--testme ast st = runState (AST.monoTraverseM AliasAnalysis ast) st
testme ast st = runState (aaunit ast) st


--type AACtx m lab e a = (MonadState (State.State a) m, Enumerated lab, AST.DefaultTraversal AliasAnalysis m e e)

--instance AACtx m lab e a => AST.Traversal AliasAnalysis m (LModule lab e) (LModule lab e) where
--    traverseM base x@(Label lab (Module path name body)) = State.withScope id continue -- State.regVarName id (Name.fromName name) *> State.withScope id continue
--        where continue =  mapM registerHeaders body
--                       -- *> AST.defaultTraverseM base x
--                       *> traverseMod x
--              id       = Enum.id lab

            --instance AACtx m lab e a => AST.Traversal AliasAnalysis m (Decl.LDecl lab e) (Decl.LDecl lab e) where
            --    traverseM base x@(Label lab ast) = case ast of
            --        Decl.Function path name inputs output body -> State.regVarName id (view MultiName.base name) *> State.withScope id continue
            --        _                                          -> continue
            --        where continue = AST.defaultTraverseM base x
            --              id       = Enum.id lab

aaunit (Unit mod) = Unit <$> aatest mod

aatest x@(Label lab (Module path name body)) = State.withScope id continue -- State.regVarName id (Name.fromName name) *> State.withScope id continue
        where continue =  mapM registerHeaders body
                       -- *> AST.defaultTraverseM base x
                       *> traverseMod x
              id       = Enum.id lab


registerHeaders (Label lab decl) = case decl of
    Decl.Function _ name _ _ _  -> State.regVarName  id (view MultiName.base name)
    Decl.Data     name _ cons _ -> State.regTypeName id (Name.fromName name) *> mapM_ registerCons cons
    _                           -> pure ()
    where id = Enum.id lab

registerCons (Label lab (Decl.Cons name fields)) = State.regVarName (Enum.id lab) (Name.fromName name)

traverseMod (Label lab (Module path name body)) = (Label lab) . (Module path name) <$> mapM traverseDecl body

--traverseDecl :: Decl.LDecl t String -> f (Decl.LDecl a String)
traverseDecl (Label lab decl) = fmap (Label lab) $ case decl of
    Decl.Function path name inputs output body -> do
        subparse <- parseString (unlines body) <$> (exprBlockParser <$> get)
        case subparse of
            Left e      -> fail $ show e
            Right (e,_) -> return $ Decl.Function path name [] output e
    Decl.Data        name params cons defs -> return $ Decl.Data        name params [] []
    Decl.Import      path rename targets   -> return $ Decl.Import      path rename targets
    Decl.TypeAlias   dst src               -> return $ Decl.TypeAlias   dst src
    Decl.TypeWrapper dst src               -> return $ Decl.TypeWrapper dst src

--registerClassHeaders (Label lab decl) = case decl of
--    Decl.Data       cls cons _ _ -> register' id cls cons
--    where id = Enum.id lab
--          register' id cls cons = State.regTypeName name id -- <* mapM registerConsHeaders cons
--                                  where name = view Type.name cls


    --instance (MonadState (State.State a) m, Enumerated lab) => AST.Traversal AliasAnalysis m (Label lab (Module f e)) where
    --    traverse base x@(Label lab m) = State.withScope id continue
    --        where continue = AST.defaultTraverse base x
    --              id       = Enum.id lab

    --instance (MonadState (State.State a) m, Enumerated lab) 
    --         => AST.Traversal AliasAnalysis m (Label lab (Decl.Decl f e)) where
    --    traverse base x@(Label lab ast) = case ast of
    --        --Decl.Function path name inputs output body -> State.regVarName id (view MultiName.base name) *> State.withScope id continue
    --        --_                                          -> continue
    --        _                                          -> undefined
    --        where continue = AST.defaultTraverse base x
    --              id       = Enum.id lab


--s2Decl d = case Label.element d of
--    Decl.Function path name inputs output body -> State.regVarName id (view Name.base name) *> 
--    where id = Label.label d

--    | Function    { _path    :: Path    , _fname    :: MultiName  , _inputs  :: [Arg f e]   , _output :: Maybe (RType f) , _body :: [e] }



    --vaMod :: Module -> VAPass AliasInfo
    --vaMod el@(Module.Module id cls imports classes typeAliases typeDefs fields methods modules) = do
    --    regModule el
    --    withScope id $ regVarName name id *> continue
    --    getAliasInfo
    --    where name     = el ^. Module.cls ^. Type.name
    --          continue =  pure ()
    --                   -- -- <* mapM registerDataCons classes -- register just data constructors before functions
    --                   <* mapM registerFuncHeaders methods
    --                   <* mapM registerClassHeaders classes

    --                   <* vaType cls
    --                   <* fexpMap imports
    --                   <* fexpMap classes -- register class functions before data member functions
    --                   <* fexpMap typeAliases
    --                   <* fexpMap typeDefs
    --                   <* fexpMap fields
    --                   <* fexpMap methods
    --                   <* fmodMap modules
    --          fexpMap  = mapM vaExpr
    --          fmodMap  = mapM vaMod


    ----registerDataCons :: Expr.Expr -> VAPass ()
    ----registerDataCons el = VAState.regExpr el *> case el of
    ----    --Expr.Data       {} -> withID continue
    ----    --Expr.ConD       {} -> regParentVarName name id *> continue
    ----    _                  -> continue
    ----    where continue = Expr.traverseM_ registerDataCons vaType vaPat vaLit el
    ----          withID   = VAState.withID (el ^. Expr.id)
    ----          id       = el ^.  Expr.id
    ----          name     = el ^.  Expr.name

    --registerClassHeaders :: Expr.Expr -> VAPass ()
    --registerClassHeaders cls = case cls of
    --    Expr.Data       id cls cons _ _ -> register' id cls cons
    --    Expr.DataNative id cls cons _ _ -> register' id cls cons
    --    where register' id cls cons = regTypeName name id <* mapM registerConsHeaders cons
    --                                  where name = view Type.name cls

    --registerConsHeaders :: Expr.Expr -> VAPass ()
    --registerConsHeaders (Expr.ConD id name fields) = regVarName name id



    --registerFuncHeaders :: Expr.Expr -> VAPass ()
    --registerFuncHeaders el = regExpr el *> case el of
    --    Expr.Function   id _ name _ _ _ -> regVarName (Name.unified name) id
    --    where continue = Expr.traverseM_ registerFuncHeaders vaType vaPat vaLit pure el