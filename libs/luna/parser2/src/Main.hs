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
import           Flowbox.Prelude              hiding (noneOf)
import qualified Luna.Data.ASTInfo            as ASTInfo
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

indentSegment p = many (checkIndent >> p)

indentBlock p = spaces *> indented *> withPos (indentSegment p)


block = string "a" <|> (foldl (++) "" <$> (char ':' *> spaces *> indentBlock block))




getColumn = column <$> position

mapIndent f err = do
  col <- getColumn
  s   <- Indent.get
  when (not $ col `f` view Indent.col s) $ fail err


indented          = mapIndent (>)  "not indented"
indentedOrEq      = mapIndent (>=) "not indented"
checkIndent       = mapIndent (==) "indentation doesn't match"
checkIndented     = mapIndent (>)  "indentation doesn't match"
checkIndentedOrEq = mapIndent (>=) "indentation doesn't match"




withPos p = do
  col <- getColumn
  Indent.with (set Indent.col col) p


--emptyIdents = IdentifierStyle
--  { _styleName     = "identifier"
--  , _styleStart    = letter <|> char '_'
--  , _styleLetter   = alphaNum <|> oneOf "_'"
--  , _styleReserved = set []
--  , _styleHighlight = Identifier
--  , _styleReservedHighlight = ReservedIdentifier
--  }


--pFunc           = appID Expr.Function <*  L.pDef
--                                      <*> (pExtPath            <?> "")
--                                      <*> (pVarOp <?> "function name")
--                                      <*> (pArgList pArg       <?> "function argument list")
--                                      <*> (try (L.pArrow *> pType) <|> appID Type.Unknown)
--                                      <*> (pExprBlock <|> return [])
--                                      <?> "function definition"







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


getASTInfo = view ParseState.info <$> get

putASTInfo info = modify (ParseState.info .~ info)

nextID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> nextID


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

prog = do
  pragma
  get
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
input = [r|@ 
|]


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
    print $ Pragma.names $ st ^. ParseState.conf . Config.pragmaSet
