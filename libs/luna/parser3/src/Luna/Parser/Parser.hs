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


--{-# LANGUAGE OverlappingInstances #-}

module Luna.Parser.Parser where


import           Control.Applicative
import           Control.Exception            (bracket)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM)
import qualified Data.ByteString              as B
import qualified Data.ByteString.UTF8         as UTF8
import           Data.CharSet.ByteSet         as S
import           Data.Default
import           Flowbox.Prelude              hiding (noneOf, maybe, element, cons)
import qualified Flowbox.Prelude              as Prelude
import qualified Luna.Data.ASTInfo            as ASTInfo
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
import           Luna.ASTNew.Name.Multi       (MultiName(MultiName))
import qualified Luna.ASTNew.Name.Multi       as MultiName
import qualified Luna.ASTNew.Name             as Name
import           Luna.ASTNew.Name             (TName(TName), TVName(TVName))

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


import qualified Luna.AST.Expr as Expr


import qualified Luna.ASTNew.Decl   as Decl
import           Luna.ASTNew.Decl   (Field(Field))
import qualified Luna.ASTNew.Module as Module
import           Luna.ASTNew.Module (Module(Module))
import qualified Luna.ASTNew.Label  as Label
import           Luna.ASTNew.Label  (Label(Label))
import qualified Luna.ASTNew.Type   as Type
import           Luna.ASTNew.Type   (Type)
import qualified Luna.ASTNew.Pat    as Pat
import           Luna.ASTNew.Pat    (Pat)
import qualified Luna.ASTNew.Lit    as Lit
import           Luna.ASTNew.Arg    (Arg(Arg))
import qualified Luna.ASTNew.Native as Native

import qualified Luna.ASTNew.Traversals as AST

infixl 4 <$!>



vName = Name.V <$> varOp
tName = Name.T <$> Tok.typeIdent

anyName = vName <|> tName


labeled p = do
    id <- nextID
    fmap (Label id) p


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
callList      p = Tok.parens (sepBy p Tok.separator)
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




regVarName m id = do
    ast <- m id
    State.regVarName id (AST.name ast)
    return ast


nameTok p = element $ regVarName $ \id -> p <*> pure id

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


----- Modules -----

pModule name path = Module <$> pure path 
                           <*> pure name 
                           <*> Indent.withPos (moduleBlock $ labeled moduleBody)
                    where moduleBody = choice [ imp, func, cls, typeAlias, typeWrapper ] <?> "module body"


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
defState  = def & State.conf .~ defConfig


appSt = State.conf %~ appConf

--st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")

parseGen p st = run (bundleResult (unit p)) st

--moduleParser modPath = parseGen (upToEnd $ func)
moduleParser modPath = parseGen (upToEnd $ pModule (last modPath) (init modPath))
--exprParser           = parseGen (upToEnd expr)
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

testme ast st = runState (AST.defaultTraverse AliasAnalysis ast) st

instance AST.Traversal AliasAnalysis m (Label id (Decl.Decl f e)) where
    traverse base l@(Label lab ast) = case ast of
        Decl.Function path name inputs output body -> continue
        _                                          -> continue
        where continue = AST.defaultTraverse base l


--s2Decl d = case Label.element d of
--    Decl.Function path name inputs output body -> State.regVarName id (view Name.base name) *> 
--    where id = Label.label d

--    | Function    { _path    :: Path    , _fname    :: MultiName  , _inputs  :: [Arg f e]   , _output :: Maybe (RType f) , _body :: [e] }
