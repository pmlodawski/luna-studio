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
import qualified Luna.Parser.Token            as Tok
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
import           Luna.Pragma.Pragma           (IsPragma)
import           Data.Typeable
import           Data.String.Utils            (join)
import           Luna.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Data.Char                    (isSpace)
import qualified Data.ByteString as ByteStr
import           Luna.AST.Name                (Name(Name))
import qualified Luna.AST.Name                as Name
import qualified Luna.Data.Namespace          as Namespace
import qualified Luna.Data.AliasInfo          as Alias
import qualified Luna.AST.AST                 as AST
import qualified Data.Maps                    as Map
import           Data.Maybe                   (fromJust)
import qualified Luna.AST.Arg                 as Arg
import qualified Data.List                    as List
import qualified Luna.Parser.Pragma           as Pragma
import           Luna.Parser.Unit             (Unit(Unit))
import qualified Luna.Parser.Unit             as Unit

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


unknownID = -1


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




--indentBlock p = Tok.spaces *> withPos p







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

tuple         p = Tok.parens (sepBy p Tok.separator)
qualifiedPath p = sepBy1_ng p Tok.accessor
extensionPath   = (,) <$> (((qualifiedPath Tok.typeIdent <?> "extension path") <* Tok.accessor) <|> pure [])
                      <*> (namePattern <?> "function name")

namePattern =   (Name.single <$> varOp)
            <|> Tok.parens (Name.close <$> (Name.multi <$> Tok.varIdent <*> many1 namePatSeg))

namePatSeg =   (Name.Token <$> Tok.varIdent)
           <|> (Name.Hole  <$  Tok.nameWildcard)

argList       p = try (sepBy2 p Tok.separator) <|> many p <?> "argument list"
argList'      p = braces (sepBy2 p Tok.separator) <|> ((:[]) <$> p) <?> "argument list"
callList      p = Tok.parens (sepBy p Tok.separator)
list          p = Tok.brackets (sepBy p Tok.separator)
anyIdent        = choice [ Tok.varIdent, Tok.typeIdent ]

varOp           = Tok.varIdent <|> Tok.operator



getASTInfo = view State.info <$> get

putASTInfo info = modify (State.info .~ info)

genID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> genID


--expr    = buildExpressionParser table term
--      <?> "expression"

--term    =  Tok.parens expr
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

--binary  name fun assoc = Infix (fun <$ Tok.reservedOp name) assoc
--prefix  name fun       = Prefix (fun <$ Tok.reservedOp name)
--postfix name fun       = Postfix (fun <$ Tok.reservedOp name)

binary   name fun assoc = Infix   (Tok.reservedOp name *> return fun) assoc
binaryM  name fun assoc = Infix   (Tok.reservedOp name *>        fun) assoc
prefix   name fun       = Prefix  (Tok.reservedOp name *> return fun)
prefixM  name fun       = Prefix  (Tok.reservedOp name *>        fun)
prefixfM      fun       = Prefix  (fun)
postfix  name fun       = Postfix (Tok.reservedOp name *> return fun)
postfixM name fun       = Postfix (Tok.reservedOp name *>        fun)


--operator2 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID) )

--binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)




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




regVarName m id = do
    ast <- m id
    State.regVarName id (AST.name ast)
    return ast


nameTok p = element $ regVarName $ \id -> p <*> pure id

tok p = element $ \id -> p <*> pure id


-----------------------------------------------------------
-- Definitions
-----------------------------------------------------------

--pragma           = Tok.pragma >* Tok.pragmaIdent

pragma = do
    Tok.pragma
    name <- Tok.pragmaIdent
    s <- get
    let lens  = State.conf . Config.pragmaSet
        pset  = s ^. lens
        names = Pragma.names pset
        pragmaIdent s = try (string s <* notFollowedBy alphaNum)
        simWords = findSimWords name names
        msgTip = if length simWords > 0 then ", perhaps you ment one of {" ++ join ", " simWords ++ "}"
                                        else ""
    when (not $ name `elem` names) $ fail $ "Unsupported extension: " ++ name ++ msgTip
    --name      <- choice (fmappragmaIdent names) <|>             -- <?> "language pragma [" ++ (join ", " $ take 3 names) ++ ", ...]"
    out       <- Pragma.parsePragma pset name
    let npset = s & lens .~ out
    put npset
    --return ()
    <?> "pragma"

pImport          = appID Expr.Import <*  Tok.kwImport
                                     <*> qualifiedPath anyIdent
                                     <*  Tok.indBlockBegin
                                     <*> (try (appID Expr.Wildcard <* Tok.importAll) <|> identE)
                                     <*> (     try (Just <$ Tok.kwAs <*> (anyIdent <?> "import name"))
                                           <|> pure Nothing
                                         )
                                     <?> "import"


pImportNative   = appID Expr.ImportNative  <*  Tok.kwImport <*> nativeE


pTypeAlias      = appID Expr.TypeAlias <*  Tok.kwAlias
                                       <*> typeT
                                       <*  Tok.assignment
                                       <*> typeT


pTypeDef        = appID Expr.TypeDef <*  Tok.kwType
                                     <*> typeT
                                     <*  Tok.assignment
                                     <*> typeT


pArg            = appID Expr.Arg     <*> argPattern
                                     <*> ((Just <$ Tok.assignment <*> expr) <|> pure Nothing)



func = element $ \id -> do
    Tok.kwDef
    (extPath, name) <- extensionPath
    State.regVarName id (view Name.base name)
    State.withScope id $ 
        Expr.function <$> pure extPath
                      <*> pure name
                      <*> (argList pArg <?> "function argument list")
                      <*> (try (Tok.arrow *> typeT) <|> appID Type.Unknown)
                      <*> (exprBlock <|> return [])
                      <*> pure id
                      <?> "function definition"



lambda          = appID Expr.Lambda <*> argList' pArg
                                    <*> (try (Tok.arrow *> typeT) <|> appID Type.Unknown)
                                    <*> exprBlock
                                    <?> "lambda definition"


----pData            = do L.pClass
----                      name  <- pCon                 <?> "class name"
----                      tvars <- many L.pIdentTypeVar <?> "class parameters"
----                      cls   <- appID Type.Data <*> pure name <*> pure tvars
----                      pDotBlockBegin pDataBody'
----                      --cons  <- try(pPipeBlockBegin pConD) <|> ((:[]) <$> pConDN name)
----                      appID Expr.Data <*> pure cls <*> pure cons

closeDefinition d = do
    ncons <- if length dcons == 1
              then do let tpname = d ^. (Expr.cls . Type.name)
                      State.regVarName (view Expr.id d) tpname
                      return $ [defc & Expr.name .~ tpname]
              else return $ init dcons
    return $ d & Expr.cons .~ ncons
    where dcons = d ^. Expr.cons
          defc  = last dcons


pData            = closeDefinition =<< pDataT

pDataT = element $ \id -> do
    Tok.kwClass
    name' <-  (Left  <$> Tok.betweenNative Tok.typeIdent) 
          <|> (Right <$> Tok.typeIdent)
          <?> "class name"
    let (name, cons) = case name' of
            Left  n -> (n, Expr.DataNative)
            Right n -> (n, Expr.Data)
    State.regTypeName id name
    Data.mk cons id <$> (appID Type.Data <*> (pure name)
                                         <*> (many (Tok.typeVarIdent <?> "class parameter")))
                    <*> (appID Expr.ConD <*> pure "default" <*> pure [] ) -- default constructor
                    <??$> blockBegin dataBody
                    <?> "class definition"


----pConDN      name = appID (\i -> Expr.ConD i name [] [] [])
----                                    <??$> pDotBlockBegin pDataBody
----                                    <?> "data constructor definition"

pConD = element $ \id -> do
    name <- Tok.conIdent
    State.regVarName id name
    Expr.ConD id name <$> pure []
                      <??$> blockBegin pConDBody
                      <?> "data constructor definition"


pConDBody        = pCombine Expr.addField fields


pModule name path = element $ \id -> do
                    State.withScope id (Module.mk id <$>   (appID Type.Module <*> pure name <*> pure path)
                                                     <??$> Indent.withPos (moduleBlock pModuleBody))


-- Parser translation unit.
-- Provides a global namespace when parsing module, expression etc.
unit p = do
    --FIXME[WD] : change id to datatype
    let id = -666 
    --id <- genID
    --Unit id <$> State.withScope id p
    State.withScope id p



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
                          , id                  <$ pragma
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
               <$> (sepBy1 Tok.varIdent Tok.separator)
               <*  Tok.typeDecl
               <*> typeT
               <*> (Tok.assignment *> (Just <$> expr) <|> pure Nothing)
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

exprT base =   (try (appID Expr.Assignment   <*> pattern <* (Tok.reservedOp "=")) <*> opTupleTE base)
           <|> opTupleTE base
           <?> "expression"


opE       = opTE entBaseE
opTupleTE base = tupleE $ opTE base

opTE base = buildExpressionParser optableE (appE base)

tupleE p = p <??> ((\xs x -> Expr.Tuple unknownID (x:xs)) <$ Tok.separator <*> sepBy1 p Tok.separator)

appE base = p <??> (appID (\i a s -> Expr.App i s a) <*> many1 (argE p)) where 
    p = termE base

argE p = try (appID Arg.Named <*> Tok.varIdent <* Tok.assignment <*> p) <|> (appID Arg.Unnamed <*> p)

termE base = base <??> (flip applyAll <$> many1 (termBaseE base))  ------  many1 (try $ recUpdE))


termBaseE p = choice [ try recUpdE
                     , accE
                     , callTermE p
                     ]

accBaseE  = (Tok.accessor *> varOp)

recUpdE   = appID (\id sel expr src -> Expr.RecordUpdate id src sel expr) <*> many1 accBaseE <* Tok.assignment <*> exprSimple

accE      = try(appID Expr.Accessor <*> accBaseE) -- needed by the syntax [1..10]


--accE   = do
--    exprs   <- fmap (flip Expr.Accessor) <$> accBaseE
--    exprsid <- mapM appID exprs
--    return (\x -> foldl (flip ($)) x exprsid)

parensE p = Tok.parens (p <|> (appID Expr.Tuple <*> pure [])) -- checks for empty tuple


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
                   , try (appID Expr.RefType <*  Tok.ref <*> Tok.conIdent) <* Tok.accessor <*> varOp
                   , appID Expr.Ref     <*  Tok.ref <*> entSimpleE
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

mkFuncParser func = State.withReserved (segNames segments) $ tok (Expr.app <$> tok (pure $ Expr.funcVar name) <*> argParser)
    where name          = Expr._fname func
          argExpr       = argE expr
          exprApp a b   = (++) <$> a <*> b
          segParsers    = fmap segParser segments
          argParser     = foldr exprApp (pure []) segParsers
          (Name base segments) = name

          segParser seg = case seg of
              Name.Hole    -> (:[]) <$> argExpr
              Name.Token s -> []    <$  Tok.symbol s

          segNames = segNames' []
          segNames' names s = case s of
              []   -> names
              x:xs -> case x of
                  Name.Token n -> segNames' (n:names) xs
                  Name.Hole    -> segNames' names     xs

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
        Just(AST.Expr func@(Expr.Function {})) -> mkFuncParser func
        _                                      -> tok $ Expr.var <$> pure name
                          

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

editCosts2 = EditCosts { deletionCosts      = ConstantCost 10
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
varOpE = tok $ Expr.var <$> try (Tok.parens varOp)
conE   = do
    name <- Tok.conIdent
    _ <- lookupAST name
    appID Expr.Con <*> pure name

identE = choice [ varE
                , varOpE
                , conE
                ]

---


listE = choice [ try $ appID Expr.RangeFromTo <*> opE <* Tok.range <*> opE
               , try $ appID Expr.RangeFrom   <*> opE <* Tok.range
               , opE
               ]


caseE     = appID Expr.Case <* Tok.kwCase <*> exprSimple <*> (blockBegin caseBodyE <|> return [])
caseBodyE = appID Expr.Match <*> pattern <*> exprBlock


--condE     = appID Expr.Cond <* Tok.kwIf <*> exprSimple <*> exprBlock <*> maybe (indBlockSpacesIE *> Tok.kwElse *> exprBlock)


nativeE     = Tok.betweenNative (many nativeElemE)
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

termT       = choice [ try $ Tok.parens typeT 
                     , entT 
                     ] <?> "type term"

appT        = appID Type.App      <*> appBaseT <*> many1 termT


argListT    = braces (sepBy2 typeT Tok.separator) <|> ((:[]) <$> typeSingle) <?> "type argument list"
funcT       = appID Type.Function <*> argListT <* Tok.arrow <*> typeT
varT        = appID Type.Var      <*> Tok.typeVarIdent
conT        = appID Type.Con      <*> qualifiedPath Tok.conIdent
tupleT      = appID Type.Tuple    <*> tuple typeT
listT       = appID Type.List     <*> Tok.brackets typeT
wildT       = appID Type.Unknown  <*  Tok.wildcard

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

patTup     = pattern <|> (appID Pat.Tuple <*> pure [])

patCon     = choice [ try appP
                    , termP
                    ]

argPattern = termBase termT

termP      = termBase typeT

termBase t = choice [ try (appID Pat.Grouped <*> Tok.parens patTup)
                    , try (appID Pat.Typed <*> entP <* Tok.typeDecl <*> t)
                    , entP
                    ]
              <?> "pattern term"

varP       = nameTok $ Pat.var         <$> Tok.varIdent
litP       = appID Pat.Lit         <*> literal
implTupleP = appID Pat.Tuple       <*> sepBy2 patCon Tok.separator
wildP      = appID Pat.Wildcard    <*  Tok.wildcard
recWildP   = appID Pat.RecWildcard <*  Tok.recWildcard
conP       = appID Pat.Con         <*> Tok.conIdent
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
charL   = appID Lit.Char   <*> Tok.charLiteral
stringL = appID Lit.String <*> Tok.stringLiteral
numL    = appID Lit.Number <*> Tok.numberL


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

--indBlockSpaces   = try (Tok.spaces <* Indent.checkIndent) <|> pure mempty

moduleBlock p = braceBlockBegin p <|> indBlockBody p
blockBegin  p = indBlockBegin   p <|> braceBlockBegin p

indBlockBegin  p = Tok.indBlockBegin *> indBlock p
indBlock       p = Tok.spaces *> Indent.indented *> Indent.withPos (indBlockBody p)
indBlockBody   p = (:) <$> p <*> many (indBlockPrefix p)
indBlockPrefix p = try ((try (Tok.spaces *> Indent.checkIndent) <|> try (Tok.terminator *> Tok.spaces *> Indent.checkIndentedOrEq )) *> notFollowedBy eof) *> p
--indBlockBody   p = (:) <$> p <*> many (try (Tok.spaces *> Indent.checkIndent) *> p)
indBlockSpacesIE = try (Tok.spaces <* Indent.checkIndentedOrEq) <|> pure mempty

braceBlockBegin p = Tok.braceL *> Tok.spaces *> Indent.discarded (many1 (p <* braceBlockSpaces)) <* Tok.braceR
braceBlockSpaces  = Tok.terminator *> Tok.spaces


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

appSt = State.conf %~ appConf

--st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")

parseGen p st = run (bundleResult (unit p)) st

moduleParser modPath = parseGen (upToEnd $ pModule (last modPath) (init modPath)) . appSt
exprParser           = parseGen (upToEnd expr) . appSt
patternParser        = parseGen (upToEnd pattern) . appSt
typeParser           = parseGen (upToEnd typeT) . appSt

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



