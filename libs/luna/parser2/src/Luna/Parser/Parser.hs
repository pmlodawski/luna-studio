{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Luna.Parser.Parser where


import           Control.Applicative
import           Control.Exception            (bracket)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join)
import qualified Data.ByteString              as B
import qualified Data.ByteString.UTF8         as UTF8
import           Data.CharSet.ByteSet         as S
import           Data.Default
import           Flowbox.Prelude              hiding (noneOf, maybe, element)
import qualified Flowbox.Prelude              as Prelude
import qualified Luna.Data.ASTInfo            as ASTInfo
import qualified Luna.AST.Module     as Module
import qualified Luna.AST.Pat        as Pat
import qualified Luna.AST.Type       as Type
import qualified Luna.AST.Lit.Number as Number
import qualified Luna.AST.Lit        as Lit
import qualified Luna.AST.Data       as Data
import qualified Luna.Parser.Lexer            as Lex
import qualified Luna.Parser.State            as State
import           System.Environment           (getArgs)
import           System.IO                    (IOMode (ReadMode), hClose, openFile)
import           System.IO                    (stdout)
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import           Text.RawString.QQ
import           Text.Trifecta                hiding (parseFromFile, parseByteString)
import qualified Text.Trifecta                as Trifecta
import           Text.Trifecta.Delta          as Delta
import qualified Luna.Data.Config             as Config
import qualified Luna.Pragma.Pragma           as Pragma
import           Luna.Pragma.Pragma           (Pragma)
import           Data.Typeable
import           Data.String.Utils            (join)
import           Luna.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Data.Char                    (isSpace)
import qualified Data.ByteString as ByteStr
import qualified Luna.Data.Name               as Name
import qualified Luna.Data.Namespace          as Namespace
import qualified Luna.Data.AliasInfo          as Alias
import qualified Luna.AST.AST                 as AST
import qualified Data.Maps                    as Map
import           Data.Maybe                   (fromJust)
import qualified Luna.AST.Arg                 as Arg
import qualified Data.List                    as List

import           Text.EditDistance            --(defaultEditCosts, levenshteinDistance, EditCosts, Costs(..))
import           Text.PhoneticCode.Phonix     (phonix)
import           Data.Function                (on)

import qualified Data.IntMap  as IntMap

--import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import qualified Text.Parsers.Indent as Indent

import Control.Lens hiding (noneOf, element)

import qualified Luna.AST.Expr as Expr

infixl 4 <$!>





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




--indentBlock p = Lex.spaces *> withPos p







--emptyIdents = IdentifierStyle
--  { _styleName     = "identifier"
--  , _styleStart    = letter <|> char '_'
--  , _styleLetter   = alphaNum <|> oneOf "_'"
--  , _styleReserved = set []
--  , _styleHighlight = Identifier
--  , _styleReservedHighlight = ReservedIdentifier
--  }


--arg            = appID Expr.Arg      <*> pArgPattern
--                                   <*> ((Just <$ L.pAssignment <*> expr) <|> pure Nothing)









--idents = emptyIdents { _styleName     = "identifier"
--                     , _styleStart    = letter <|> char '_'
--                     , _styleLetter   = alphaNum <|> oneOf "_'"
--                     , _styleReserved = HashSet.fromList []
--                     , _styleHighlight = Identifier
--                     , _styleReservedHighlight = ReservedIdentifier
--                     }

--haskellIdents = haskell98Idents
--  { styleLetter   = styleLetter haskell98Idents <|> () <$ char '#'
--  , styleReserved = set $ haskell98ReservedIdents ++
--      ["foreign","import","export","primitive","_ccall_","_casm_" ,"forall"]
--  }

tuple         p = Lex.parens (sepBy p Lex.separator)
qualifiedPath p = sepBy1_ng p Lex.accessor
extensionPath   = (,) <$> (((qualifiedPath Lex.typeIdent <?> "extension path") <* Lex.accessor) <|> pure [])
                      <*> (     (Name.Single <$> Lex.varIdent)
                            <|> Lex.parens (Name.Multi <$> Lex.varIdent <*> many1 Lex.varIdent) 
                            <?> "function name")
argList       p = try (sepBy2 p Lex.separator) <|> many p <?> "argument list"
argList'      p = braces (sepBy2 p Lex.separator) <|> ((:[]) <$> p) <?> "argument list"
callList      p = Lex.parens (sepBy p Lex.separator)
list          p = Lex.brackets (sepBy p Lex.separator)
anyIdent        = choice [ Lex.varIdent, Lex.typeIdent ]

varOp           = Lex.varIdent <|> Lex.operator



getASTInfo = view State.info <$> get

putASTInfo info = modify (State.info .~ info)

genID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> genID


--expr    = buildExpressionParser table term
--      <?> "expression"

--term    =  Lex.parens expr
--      <|> natural
--      <?> "simple expression"

--table   = [ 
--            [operator2 "*"                                  AssocLeft]
--            --[binary "" (+) AssocLeft]
--          --, [prefix "-" negate, prefix "+" id ]
--          --, [postfix "++" (+1)]
--          --, [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
--          --, [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
--          ]

--binary  name fun assoc = Infix (fun <$ Lex.reservedOp name) assoc
--prefix  name fun       = Prefix (fun <$ Lex.reservedOp name)
--postfix name fun       = Postfix (fun <$ Lex.reservedOp name)

binary   name fun assoc = Infix   (Lex.reservedOp name *> return fun) assoc
binaryM  name fun assoc = Infix   (Lex.reservedOp name *>        fun) assoc
prefix   name fun       = Prefix  (Lex.reservedOp name *> return fun)
prefixM  name fun       = Prefix  (Lex.reservedOp name *>        fun)
prefixfM      fun       = Prefix  (fun)
postfix  name fun       = Postfix (Lex.reservedOp name *> return fun)
postfixM name fun       = Postfix (Lex.reservedOp name *>        fun)


--operator2 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID) )

--binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)

pragma = do
    token $ char '@'
    s <- get
    let lens  = State.conf . Config.pragmaSet
        pset  = s ^. lens
        names = Pragma.names pset
    name      <- choice (fmap string names) <?> "language pragma [" ++ (join ", " $ take 3 names) ++ ", ...]"
    out       <- Pragma.parsePragma pset name
    let npset = s & lens .~ out
    put npset
    <?> "pragma"


--pragma2 = pragma <?> "pragma"

--tst :: Indent.IndentStateT Int (State Char) Int
--tst = do
--    get
--    Indent.get

--element :: (Luna.AST.Common.ID -> m0 b0) -> m0 b0



element m = do
    id <- genID
    State.registerID id
    ast <- m id
    State.registerAST id ast
    return ast

--zrobic scope kopiowany! nie parentowany. przeciez jak w parencie cos sie zmieni to bez sensu by sie propagowalo nizej jak juz jestesmy PO funkcji

--element m = do
--    id <- genID
--    pid <- Namespace.head . view State.namespace <$> get
--    mapStateVal (State.namespace . Namespace.alias %~ Alias.regParent id pid)
--    ast <- m id
--    mapStateVal (State.namespace . Namespace.alias %~ Alias.regAST    id ast)
--    return ast


container m = element $ \id -> State.withScope id $ m id




registerName m id = do
    ast <- m id
    State.registerName id (AST.name ast)
    return ast


nameTok p = element $ registerName $ \id -> p <*> pure id

tok p = element $ \id -> p <*> pure id


-----------------------------------------------------------
-- Definitions
-----------------------------------------------------------
pImport          = appID Expr.Import <*  Lex.kwImport
                                     <*> qualifiedPath anyIdent
                                     <*  Lex.indBlockBegin
                                     <*> (try (appID Expr.Wildcard <* Lex.importAll) <|> identE)
                                     <*> (     try (Just <$ Lex.kwAs <*> (anyIdent <?> "import name"))
                                           <|> pure Nothing
                                         )
                                     <?> "import"


pImportNative   = appID Expr.ImportNative  <*  Lex.kwImport <*> nativeE


pTypeAlias      = appID Expr.TypeAlias <*  Lex.kwAlias
                                       <*> typeT
                                       <*  Lex.assignment
                                       <*> typeT


pTypeDef        = appID Expr.TypeDef <*  Lex.kwType
                                     <*> typeT
                                     <*  Lex.assignment
                                     <*> typeT


pArg            = appID Expr.Arg     <*> argPattern
                                     <*> ((Just <$ Lex.assignment <*> expr) <|> pure Nothing)



func = element $ \id -> do
    Lex.kwDef
    (extPath, name) <- extensionPath
    State.registerName id (view Name.base name)
    State.withScope id $ 
        Expr.function <$> pure extPath
                      <*> pure name
                      <*> (argList pArg <?> "function argument list")
                      <*> (try (Lex.arrow *> typeT) <|> appID Type.Unknown)
                      <*> (exprBlock <|> return [])
                      <*> pure id
                      <?> "function definition"



lambda          = appID Expr.Lambda <*> argList' pArg
                                    <*> (try (Lex.arrow *> typeT) <|> appID Type.Unknown)
                                    <*> exprBlock
                                    <?> "lambda definition"


----pData            = do L.pClass
----                      name  <- pCon                 <?> "class name"
----                      tvars <- many L.pIdentTypeVar <?> "class parameters"
----                      cls   <- appID Type.Data <*> pure name <*> pure tvars
----                      pDotBlockBegin pDataBody'
----                      --cons  <- try(pPipeBlockBegin pConD) <|> ((:[]) <$> pConDN name)
----                      appID Expr.Data <*> pure cls <*> pure cons


pData            = Expr.afterData <$> pDataT

pDataT = element $ \id -> do
    Lex.kwClass
    name <- Lex.typeIdent <?> "class name"
    State.registerName id name
    Data.mk id <$> (appID Type.Data <*> (pure name)
                                       <*> (many (Lex.typeVarIdent <?> "class parameter")))
                  <*> (appID Expr.ConD <*> pure "default" <*> pure [] ) -- default constructor
                  <??$> blockBegin dataBody
                  <?> "class definition"


----pConDN      name = appID (\i -> Expr.ConD i name [] [] [])
----                                    <??$> pDotBlockBegin pDataBody
----                                    <?> "data constructor definition"

pConD            = appID Expr.ConD    <*> Lex.conIdent
                                    <*> pure []
                                    <??$> blockBegin pConDBody
                                    <?> "data constructor definition"


pConDBody        = pCombine Expr.addField fields


pModule name path = do
                    id <- genID
                    mapStateVal (State.namespace %~ Namespace.pushScope id)
                    ret <- Module.mk id <$>   (appID Type.Module <*> pure name <*> pure path)
                                        <??$> Indent.withPos (moduleBlock pModuleBody)
                    mapStateVal (State.namespace %~ Namespace.popScope)
                    return ret



dataBody       = choice [ Expr.addMethod <$> func
                         , pCombine Expr.addFieldDC fields
                         , Expr.addClass  <$> pData
                         , Expr.addCon    <$> pConD
                         ]
                <?> "class body"

pModuleBody      = choice [ Module.addMethod    <$> func
                          , pCombine Module.addField fields
                          , Module.addClass     <$> pData
                          , Module.addImport    <$> try(pImportNative)
                          , Module.addImport    <$> pImport
                          , Module.addTypeAlias <$> pTypeAlias
                          , Module.addTypeDef   <$> pTypeDef
                          ]
                <?> "definition"

pCombine f p = do
    fs <- map (\x -> f <$> x) <$> p
    foldl (liftA2 (.)) (pure Prelude.id) fs


--pField         =   appID Expr.Field
--               <*> L.pIdent
--               <*  L.pTypeDecl
--               <*> pType
--               <*> (L.pAssignment *> (Just <$> pExpr) <|> pure Nothing)

mkField t val name id = Expr.Field id name t val

fields        =   (\names t val -> map (appID . (mkField t val)) names )
               <$> (sepBy1 Lex.varIdent Lex.separator)
               <*  Lex.typeDecl
               <*> typeT
               <*> (Lex.assignment *> (Just <$> expr) <|> pure Nothing)
               <?> "field declaration"

declaration   = choice [ pImport
                        , func
                        , pData
                        , try $ lambda
                        ]

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
expr       = exprT entBaseE

exprSimple = exprT pEntBaseSimpleE

exprT base =   (try (appID Expr.Assignment   <*> pattern <* (Lex.reservedOp "=")) <*> opTupleTE base)
           <|> opTupleTE base
           <?> "expression"


opE       = opTE entBaseE
opTupleTE base = tupleE $ opTE base

opTE base = buildExpressionParser optableE (appE base)

tupleE p = p <??> ((\xs x -> Expr.Tuple 0 (x:xs)) <$ Lex.separator <*> sepBy1 p Lex.separator)

appE base = p <??> (appID (\i a s -> Expr.App i s a) <*> many1 (argE p)) where 
    p = termE base

argE p = try (appID Arg.Named <*> Lex.varIdent <* Lex.assignment <*> p) <|> (appID Arg.Unnamed <*> p)

termE base = base <??> (flip applyAll <$> many1 (termBaseE base))  ------  many1 (try $ recUpdE))


termBaseE p = choice [ try recUpdE
                     , accE
                     , callTermE p
                     ]

accBaseE  = (Lex.accessor *> varOp)

recUpdE   = appID (\id sel expr src -> Expr.RecordUpdate id src sel expr) <*> many1 accBaseE <* Lex.assignment <*> exprSimple

accE      = try(appID Expr.Accessor <*> accBaseE) -- needed by the syntax [1..10]


--accE   = do
--    exprs   <- fmap (flip Expr.Accessor) <$> accBaseE
--    exprsid <- mapM appID exprs
--    return (\x -> foldl (flip ($)) x exprsid)

parensE p = Lex.parens (p <|> (appID Expr.Tuple <*> pure [])) -- checks for empty tuple


callTermE p = lastLexemeEmpty *> ((flip <$> appID Expr.App) <*> callList (argE p))


entBaseE        = entConsE entComplexE
pEntBaseSimpleE = entConsE entSimpleE

entConsE base = choice [ try $ appID Expr.Grouped <*> parensE (exprT base)
                       , base
                       ]

entComplexE = choice[ declaration
                    , entSimpleE
                    ]
             <?> "expression term"

entSimpleE = choice[ caseE -- CHECK [wd]: removed try
                   --, condE
                   , appID Expr.Grouped <*> parensE expr
                   , identE
                   , try (appID Expr.RefType <*  Lex.ref <*> Lex.conIdent) <* Lex.accessor <*> varOp
                   , appID Expr.Ref     <*  Lex.ref <*> entSimpleE
                   , appID Expr.Lit     <*> literal
                   , appID Expr.List    <*> list  listE
                   , appID Expr.Native  <*> nativeE
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
           , [ binaryM  "$"  (callBuilder <$> genID <*> genID)      AssocLeft ]
           , [ postfixM  "::" (appID Expr.Typed <*> typeT)                      ]
           --, [ binaryM   ","  (appID Expr.tupleBuilder)    AssocLeft ]
           ]
           where
              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
              operator4 op = binaryM op ( (\id1 id2 id3 id4 x y -> Expr.App id1 (Expr.Var id2 op) [Arg.Unnamed id3 x, Arg.Unnamed id4 y]) <$> genID <*> genID <*> genID <*> genID)
              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure ('~':op)))
              --operator2 op = binaryM op (binaryMatchE <$>  ( appID Expr.App <*> (appID Expr.Accessor <*> pure "add" <*> ... ) )  )
              --operator2 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID)
              --operator3 op = binaryM op ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 "contains" y) [x]) <$> genID <*> genID)


--callBuilder :: ID -> Expr -> Expr -> Expr
callBuilder id id2 src arg = case src of
    Expr.App id' src' args -> Expr.App id' src' (args ++ [Arg.Unnamed id2 arg])
    _                      -> Expr.App id src [Arg.Unnamed id2 arg]


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

withReservedWords words p = withState (State.addReserved words) p


mkFuncParser func = withReservedWords segments $ tok (Expr.app <$> tok (pure $ Expr.var fname) <*> argParser)
    where argExpr       = argE expr
          exprApp p a b = (:) <$> p <* a <*> b
          segParsers    = fmap (Lex.symbol) segments
          argParser     = foldr (exprApp argExpr) ((:[]) <$> argExpr) segParsers
          (Name.Multi base segments) = Expr._fname func
          [s1,s2] = fmap Lex.symbol segments
          fname = base ++ " " ++ join " " segments



notReserved p = do
    rsv  <- view State.adhocReserved <$> get
    name <- p
    if name `elem` rsv then fail $ "'" ++ name ++ "' is a reserved word"
                       else return name


---
varE   = do
    name  <- try $ notReserved Lex.varIdent
    ast <- lookupAST name
    case ast of
        -- FIXME[wd]: dopiero przy dwuprzebiegowym parserze bedziemy mieli wieloczlonowe funkcje rekurencyjne
        Just(AST.Expr func@(Expr.Function {})) -> mkFuncParser func
        _                                      -> tok $ Expr.var <$> pure name
                          

lookupAST name = do
    scope  <- State.getScope
    astMap <- State.getASTMap
    pid    <- State.getPid
        
    case Map.lookup pid scope of
            Nothing                    -> fail "Internal parser error [1]"
            Just (Alias.Scope nameMap) -> case Map.lookup name nameMap of
                Just dstID -> return $ Map.lookup dstID astMap -- FIXME[wd]: zwracamy maybe. Nothing zostanie zwrocone przy rekurencji. Poprawic przy dwuprzebiegowym parserze
                Nothing    -> fail $ "name '" ++ name ++ "' is not defined" ++ msgTip
                    where scopedNames = Map.keys nameMap
                          simWords    = findSimWords name scopedNames
                          msgTip = if length simWords > 0 then ", perhaps you ment one of {" ++ join ", " (fmap show simWords) ++ "}"
                                                          else ""
                          

editCosts = EditCosts { deletionCosts      = ConstantCost 10
                      , insertionCosts     = ConstantCost 10
                      , substitutionCosts  = ConstantCost 10
                      , transpositionCosts = ConstantCost 10
                      }

findSimWords word words = fmap snd simPairs
    where dist a b = levenshteinDistance editCosts (phonix a) (phonix b)
    --where dist a b = levenshteinDistance editCosts a b
          simWords = fmap (dist word) words
          simPairs = filter ((<20).fst) 
                   $ List.sortBy (compare `on` fst) 
                   $ zip simWords words


    
--varE   = appID $ Expr.var <*> Lex.varIdent
varOpE = tok $ Expr.var <$> try (Lex.parens varOp)
conE   = do
    name <- Lex.conIdent
    _ <- lookupAST name
    appID Expr.Con <*> pure name

identE = choice [ varE
                , varOpE
                , conE
                ]

---


listE = choice [ try $ appID Expr.RangeFromTo <*> opE <* Lex.range <*> opE
               , try $ appID Expr.RangeFrom   <*> opE <* Lex.range
               , opE
               ]


caseE     = appID Expr.Case <* Lex.kwCase <*> exprSimple <*> (blockBegin caseBodyE <|> return [])
caseBodyE = appID Expr.Match <*> pattern <*> exprBlock


--condE     = appID Expr.Cond <* Lex.kwIf <*> exprSimple <*> exprBlock <*> maybe (indBlockSpacesIE *> Lex.kwElse *> exprBlock)


nativeE     = between Lex.nativeSym Lex.nativeSym (many nativeElemE)
nativeElemE = choice [ nativeVarE
                     , nativeCodeE
                     ]
nativeCodeE = appID Expr.NativeCode <*> ((:) <$> (noneOf "`#") <*> nativeCodeBodyE)
nativeVarE  = appID Expr.NativeVar  <*  symbol "#{" <*> many (noneOf "}") <* symbolic '}'

nativeCodeBodyE = (try(lookAhead $ string "#{")  *> pure [])
              <|> (try(lookAhead $ string "```") *> pure [])
              <|> ((++) <$> ((:) <$> anyChar <*> many (noneOf "`#")) <*> nativeCodeBodyE)


exprBlock  = blockBegin expr

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

typeT       = choice [ try funcT
                     , typeSingle
                     ] <?> "type"

typeSingle  = choice [ try appT
                     , termT 
                     ] <?> "type"

termT       = choice [ try $ Lex.parens typeT 
                     , entT 
                     ] <?> "type term"

appT        = appID Type.App      <*> appBaseT <*> many1 termT


argListT    = braces (sepBy2 typeT Lex.separator) <|> ((:[]) <$> typeSingle) <?> "type argument list"
funcT       = appID Type.Function <*> argListT <* Lex.arrow <*> typeT
varT        = appID Type.Var      <*> Lex.typeVarIdent
conT        = appID Type.Con      <*> qualifiedPath Lex.conIdent
tupleT      = appID Type.Tuple    <*> tuple typeT
wildT       = appID Type.Unknown  <*  Lex.wildcard

appBaseT    = choice [ varT, conT
                     ]

entT        = choice [ varT
                     , conT
                     , tupleT
                     , wildT
                     ]


-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------
pattern    = choice [ try implTupleP
                    , patCon
                    ]

patTup     = pattern <|> (appID Pat.Tuple <*> pure [])

patCon     = choice [ try appP
                    , termP
                    ]

argPattern = termBase termT

termP      = termBase typeT

termBase t = choice [ try (appID Pat.Grouped <*> Lex.parens patTup)
                    , try (appID Pat.Typed <*> entP <* Lex.typeDecl <*> t)
                    , entP
                    ]
              <?> "pattern term"

varP       = nameTok $ Pat.var         <$> Lex.varIdent
litP       = appID Pat.Lit         <*> literal
implTupleP = appID Pat.Tuple       <*> sepBy2 patCon Lex.separator
wildP      = appID Pat.Wildcard    <*  Lex.wildcard
recWildP   = appID Pat.RecWildcard <*  Lex.recWildcard
conP       = appID Pat.Con         <*> Lex.conIdent
appP       = appID Pat.App         <*> conP <*> many1 termP

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
charL   = appID Lit.Char   <*> Lex.charLiteral
stringL = appID Lit.String <*> Lex.stringLiteral
numL    = appID Lit.Number <*> Lex.numberL


prevParsedChar = do
    Caret delta bs <- careting
    --return ' '
    return $ (UTF8.toString bs !! (max 0 . fromIntegral $ column delta - 1))

lastLexemeEmpty = do
    prevChar <- prevParsedChar
    when (isSpace prevChar) $ fail "not empty"


----------------------------------------------------------------------
-- Code segments
----------------------------------------------------------------------
--indBlockBody   p = many1 (Indent.checkIndentedOrEq *> p <* indBlockSpaces)

--indBlockSpaces   = try (Lex.spaces <* Indent.checkIndent) <|> pure mempty

moduleBlock p = braceBlockBegin p <|> indBlockBody p
blockBegin  p = indBlockBegin   p <|> braceBlockBegin p

indBlockBegin  p = Lex.indBlockBegin *> indBlock p
indBlock       p = Lex.spaces *> Indent.indented *> Indent.withPos (indBlockBody p)
indBlockBody   p = (:) <$> p <*> many (indBlockPrefix p)
indBlockPrefix p = try ((try (Lex.spaces *> Indent.checkIndent) <|> try (Lex.terminator *> Lex.spaces *> Indent.checkIndentedOrEq )) *> notFollowedBy eof) *> p
--indBlockBody   p = (:) <$> p <*> many (try (Lex.spaces *> Indent.checkIndent) *> p)
indBlockSpacesIE = try (Lex.spaces <* Indent.checkIndentedOrEq) <|> pure mempty

braceBlockBegin p = Lex.braceL *> Lex.spaces *> Indent.discarded (many1 (p <* braceBlockSpaces)) <* Lex.braceR
braceBlockSpaces  = Lex.terminator *> Lex.spaces


-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

parserName = "Luna Compiler"

run p st = evalStateT (Indent.parser p) st

handleResult r = case r of
    Failure e -> Left e
    Success a -> Right a

bundleResult p = (,) <$> p <*> get

end = (Lex.spaces <?> "") <* (eof <?> "")

upToEnd p = Lex.spaces *> p <* end

renderErr e = renderPretty 0.8 80 $ e <> linebreak

-----------------------------------------------------------
-- Pragmas
-----------------------------------------------------------

data TabLength = TabLength Int   deriving (Show, Typeable, Read)
data ImplicitSelf = ImplicitSelf deriving (Show, Typeable, Read)

instance Pragma TabLength
instance Pragma ImplicitSelf

conf = def & Config.registerPragma (undefined :: TabLength)
           & Config.registerPragma (undefined :: ImplicitSelf)

appConf = Config.registerPragma (undefined :: TabLength)
        . Config.registerPragma (undefined :: ImplicitSelf)

appSt = State.conf %~ appConf

st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")

parseGen p st = run (bundleResult p) st

moduleParser modPath = parseGen (upToEnd $ pModule (last modPath) (init modPath)) . appSt
exprParser           = parseGen expr  . appSt
typeParser           = parseGen typeT . appSt


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



