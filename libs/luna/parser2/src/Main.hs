{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

module Main (main) where


import           Control.Applicative
import           Control.Exception            (bracket)
import           Control.Monad.State          hiding (mapM_, (<$!>), join)
import qualified Data.ByteString              as B
import           Data.ByteString.UTF8         as UTF8 hiding (foldl, length, take)
import           Data.CharSet.ByteSet         as S
import           Data.Default
import           Flowbox.Prelude              hiding (noneOf, maybe)
import qualified Luna.Data.ASTInfo            as ASTInfo
import qualified Luna.AST.Module     as Module
import qualified Luna.AST.Pat        as Pat
import qualified Luna.AST.Type       as Type
import qualified Luna.AST.Lit.Number as Number
import qualified Luna.AST.Lit        as Lit
import qualified Luna.Parser.Lexer            as Lex
import qualified Luna.Parser.State            as ParseState
import           System.Environment           (getArgs)
import           System.IO                    (IOMode (ReadMode), hClose, openFile)
import           System.IO                    (stdout)
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import           Text.RawString.QQ
import           Text.Trifecta 
import           Text.Trifecta.Delta          as Delta
import qualified Luna.Data.Config             as Config
import qualified Luna.Pragma.Pragma           as Pragma
import           Luna.Pragma.Pragma           (Pragma)
import           Data.Typeable
import           Data.String.Utils            (join)
import           Luna.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead


--import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import qualified Text.Parsers.Indent as Indent

import Control.Lens hiding (noneOf)

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


--indentBlock p = spaces *> withPos p







--emptyIdents = IdentifierStyle
--  { _styleName     = "identifier"
--  , _styleStart    = letter <|> char '_'
--  , _styleLetter   = alphaNum <|> oneOf "_'"
--  , _styleReserved = set []
--  , _styleHighlight = Identifier
--  , _styleReservedHighlight = ReservedIdentifier
--  }


--arg            = tok Expr.Arg      <*> pArgPattern
--                                   <*> ((Just <$ L.pAssignment <*> expr) <|> pure Nothing)


func           = do
    Lex.kwDef
    (extPath, name) <- extensionPath
    appID Expr.Function <*> pure extPath
                        <*> pure name
                        <*> pure []
                        <*> appID Type.Unknown
                        <*> pure []
                        <?> "function definition"







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

tuple         p = parens (sepBy p Lex.separator)
qualifiedPath p = sepBy1_ng p Lex.accessor
extensionPath   = (,) <$> ((qualifiedPath Lex.typeIdent <* Lex.accessor) <|> pure []) <*> (Lex.varIdent <?> "function name")
argList       p = try (sepBy2 p Lex.separator) <|> many p <?> "argument list"
argList'      p = try (sepBy2 p Lex.separator) <|> ((:[]) <$> p) <?> "argument list"


getASTInfo = view ParseState.info <$> get

putASTInfo info = modify (ParseState.info .~ info)

nextID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> nextID


expr    = buildExpressionParser table term
      <?> "expression"

term    =  parens expr
      <|> natural
      <?> "simple expression"

table   = [ [binary "" (+) AssocLeft]
          , [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)]
          , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
          , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
          ]

binary  name fun assoc = Infix (fun <$ Lex.reservedOp name) assoc
prefix  name fun       = Prefix (fun <$ Lex.reservedOp name)
postfix name fun       = Postfix (fun <$ Lex.reservedOp name)


pragma = do
    token $ char '@'
    s <- get
    let lens  = ParseState.conf . Config.pragmaSet
        pset  = s ^. lens
        names = Pragma.names pset
    name      <- choice (fmap string names) <?> "language pragma [" ++ (join ", " $ take 3 names) ++ ", ...]"
    out       <- Pragma.parsePragma pset name
    let npset = s & lens .~ out
    put npset


--pragma2 = pragma <?> "pragma"

--tst :: Indent.IndentStateT Int (State Char) Int
--tst = do
--    get
--    Indent.get



varOp       = Lex.varIdent <|> Lex.operator

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------
--expr       = exprT entBaseE

--exprSimple = exprT pEntBaseSimpleE

--exprT base =   --(try (appID Expr.RecordUpdate <*> pVar <*> many1 (L.pAccessor *> pVar) <* (L.reservedOp "=")) <*> opTE base)
--            (try (appID Expr.Assignment   <*> pPattern <* (L.reservedOp "=")) <*> opTE base)
--            <|> opTE base
--            <?> "expression"


--opE       = opTE entBaseE
--opTE base = Expr.aftermatch <$> PExpr.buildExpressionParser optableE (termE base)

termE base = base <??> (flip applyAll <$> many1 (termBaseE base))  ------  many1 (try $ termRecUpd))


termBaseE p = choice [ --try termRecUpd
                      dotTermE
                      --, pCallTermE p
                      ]

dotTermBase  = (Lex.accessor *> varOp)

--termRecUpd   = appID (\id sel expr src -> Expr.RecordUpdate id src sel expr) <*> many1 dotTermBase <* L.pAssignment <*> exprSimple

dotTermE     = try(appID Expr.Accessor <*> dotTermBase) -- needed by the syntax [1..10]


--dotTermE   = do
--    exprs   <- fmap (flip Expr.Accessor) <$> dotTermBase
--    exprsid <- mapM appID exprs
--    return (\x -> foldl (flip ($)) x exprsid)


--pCallTermE p = lastLexemeEmpty *> ((flip <$> appID Expr.App) <*> callList p)


entBaseE       = entConsE entComplexE
pEntBaseSimpleE = entConsE entSimpleE

entConsE base = choice [ --try $ appID Expr.Grouped <*> L.parensed (exprT base)
                        base
                        ]

entComplexE = choice[ --declaration
                     entSimpleE
                     ]
             <?> "expression term"

entSimpleE = choice[ --caseE -- CHECK [wd]: removed try
                    --condE
                    --try $ appID Expr.Grouped <*> parens expr
                    identE
                    , try (appID Expr.RefType <*  Lex.ref <*> Lex.conIdent) <* Lex.accessor <*> varOp
                    , appID Expr.Ref     <*  Lex.ref <*> entSimpleE
                    , appID Expr.Lit     <*> literal
                    --, appID Expr.Tuple   <*> pTuple  opE
                    --, appID Expr.List    <*> pList   listE
                    , appID Expr.Native  <*> nativeE
                    ]
           <?> "expression term"

--optableE = [ [ postfixM  "::" (appID Expr.Typed <*> pType)                      ]
--           --, [ prefixM   "@"  (appID Expr.Ref)                                  ]
--           , [ binaryM   ""   (appID Expr.callConstructor)      PExpr.AssocLeft ]
--           , [ operator2 "^"                                  PExpr.AssocLeft ]
--           , [ operator2 "*"                                  PExpr.AssocLeft ]
--           , [ operator2 "/"                                  PExpr.AssocLeft ]
--           , [ operator2 "+"                                  PExpr.AssocLeft ]
--           , [ operator2 "-"                                  PExpr.AssocLeft ]
--           , [ operator2 "<"                                  PExpr.AssocLeft ]
--           , [ operator2 ">"                                  PExpr.AssocLeft ]
--           , [ operator2 "=="                                 PExpr.AssocLeft ]
--           , [ operator2 "in"                                 PExpr.AssocLeft ]
--           , [ binaryM  "$"  (binaryMatchE <$> appID Expr.callConstructor)      PExpr.AssocLeft ]
--           ]
--           where
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure op))
--              operator4 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Var id2 op) [x, y]) <$> genID <*> genID) )
--              --operator op = binaryM op (binaryMatchE <$> (appID Expr.Infix <*> pure ('~':op)))
--              --operator2 op = binaryM op (binaryMatchE <$>  ( appID Expr.App <*> (appID Expr.Accessor <*> pure "add" <*> ... ) )  )
--              operator2 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 op x) [y]) <$> genID <*> genID) )
--              operator3 op = binaryM op (binaryMatchE <$> ( (\id1 id2 x y -> Expr.App id1 (Expr.Accessor id2 "contains" y) [x]) <$> genID <*> genID) )


--binaryM2  name fun assoc = PExpr.Infix   (L.reserved name *>        fun) assoc

--binaryMatchE  f p q = f   (Expr.aftermatch p) (Expr.aftermatch q)



---
varE   = appID Expr.Var <*> Lex.varIdent
varOpE = appID Expr.Var <*> parens varOp
conE   = appID Expr.Con <*> Lex.conIdent

identE = choice [ varE
                , varOpE
                , conE
                ]

---


--listE = choice [ try $ appID Expr.RangeFromTo <*> opE <* L.pRange <*> opE
--                   , try $ appID Expr.RangeFrom   <*> opE <* L.pRange
--                   , opE
--                   ]


--caseE     = appID Expr.Case <* Lex.kwCase <*> exprSimple <*> (pDotBlockBegin caseBodyE <|> return [])
--caseBodyE = appID Expr.Match <*> pPattern <*> exprBlock


--condE     = appID Expr.Cond <* Lex.kwIf <*> exprSimple <*> exprBlock <*> maybe (blockSpacesIE *> Lex.kwElse *> exprBlock)


nativeE     = between Lex.nativeSym Lex.nativeSym (many nativeElemE)
nativeElemE = choice [ nativeVarE
                     , nativeCodeE
                     ]
nativeCodeE = appID Expr.NativeCode <*> ((:) <$> (noneOf "`#") <*> nativeCodeBodyE)
nativeVarE  = appID Expr.NativeVar  <*  symbol "#{" <*> many (noneOf "}") <* symbolic '}'

nativeCodeBodyE = (try(lookAhead $ string "#{")  *> pure [])
              <|> (try(lookAhead $ string "```") *> pure [])
              <|> ((++) <$> ((:) <$> anyChar <*> many (noneOf "`#")) <*> nativeCodeBodyE)


--exprBlock  = pDotBlockBegin expr

-----------------------------------------------------------
-- Types
-----------------------------------------------------------

typeT       = choice [ try funcT
                     , typeSingle
                     ] <?> "type"

typeSingle  = choice [ try appT
                     , termT 
                     ] <?> "type"

termT       = choice [ try $ parens typeT 
                     , entT 
                     ] <?> "type term"

appT        = appID Type.App      <*> appBaseT <*> many1 termT
funcT       = appID Type.Function <*> argList' typeSingle <* Lex.arrow <*> typeT
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

patCon     = choice [ try appP
                    , termP
                    ]

argPattern = termBase typeSingle

termP      = termBase typeT

termBase t = choice [ try $ parens patCon
                    , try (appID Pat.Typed <*> entP <* Lex.typeDecl <*> t)
                    , entP
                    ]
              <?> "pattern term"

varP       = appID Pat.Var         <*> Lex.varIdent
litP       = appID Pat.Lit         <*> literal
tupleP     = appID Pat.Tuple       <*> tuple patCon
implTupleP = appID Pat.Tuple       <*> sepBy2 patCon Lex.separator
wildP      = appID Pat.Wildcard    <*  Lex.wildcard
recWildP   = appID Pat.RecWildcard <*  Lex.recWildcard
conP       = appID Pat.Con         <*> Lex.conIdent
appP       = appID Pat.App         <*> conP <*> many1 termP

entP = choice [ varP
              , litP
              , tupleP
              , wildP
              , recWildP
              , conP
              ]

----------------------------------------------------------------------
-- Literals
----------------------------------------------------------------------

literal = choice [ numL, charL, stringL ]
charL   = appID Lit.Char   <*> charLiteral
stringL = appID Lit.String <*> Lex.stringLiteral
numL    = appID Lit.Number <*> Lex.numberL





prog = do
    pattern
    --func
    --pragma
    --get
    --Indent.get
    --get
    --many $ char 'a'
    --withPos prog2
    --block
    --getColumn
    --indented
    --Lex.variable
    --char 'a'
    --reserved

--prog2 = do
--  many $ char 'b'
--  indented
--  get

input :: String
input = [r|a,b,c
|]

--input = [r|a -> Vector Int -> _

lumpy arg = do
  --r <- parseFromFile (many request) arg
  let r = parseByteString (evalStateT (Indent.parser prog) st) (Directed (UTF8.fromString "Luna compiler") 0 0 0 0) (UTF8.fromString input)
  --let r = parseByteString (prog) (Directed (UTF8.fromString "ala") 0 0 0 0) (UTF8.fromString "ala")

  case r of
     Failure xs -> do displayIO stdout $ renderPretty 0.8 80 $ xs <> linebreak
                      putStrLn $ "Type 'luna --help' to get more information about syntax and possible options."
     Success a  -> print (a)
  --case r of
  --  Nothing -> return ()
  --  Just rs -> print (length rs)



--sign :: TokenParsing m => m (Integer -> Integer)



--number = Number.decimal <*> sign <*> numScope

--numDecimal = Number.decimal <*> sign <*> pure (Number.Decimal "") <*> pure Nothing
--numOct     = Number.octodecimal <$> sign <*> (char '0' *> (char 'o' <|> char 'O'))
--numDecimal = Number.decimal <$> sign <*> numRepr digit <*> numExp



data TabLength = TabLength Int   deriving (Show, Typeable, Read)
data ImplicitSelf = ImplicitSelf deriving (Show, Typeable, Read)

instance Pragma TabLength
instance Pragma ImplicitSelf

conf = def & Config.registerPragma (undefined :: TabLength)
           & Config.registerPragma (undefined :: ImplicitSelf)

st = def {ParseState._conf = conf}


main :: IO ()
main = do
    mapM_ lumpy =<< return ["Test.txt"]
    --print conf
    --print $ Pragma.names $ st ^. ParseState.conf . Config.pragmaSet
