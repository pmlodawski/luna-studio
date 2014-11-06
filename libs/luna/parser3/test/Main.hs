import System.Environment (getArgs)
import Flowbox.Prelude
import qualified Luna.Parser.Parser as Parser
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import Data.Default
import           System.IO                    (stdout)
import           Text.Show.Pretty
import qualified Luna.Parser.State as State
import qualified Luna.Parser.State as ParserState

import qualified Luna.Data.ASTInfo  as ASTInfo
import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Data.Config   as Config
import           Luna.ASTNew.Name   (TName(TName))

patchedParserState info' = def
    & ParserState.info .~ info'
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


--parsePattern    pat  = Parser.parseString pat  $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0)
--parseExpression expr = Parser.parseString expr $ Parser.exprParser    (patchedParserState $ ASTInfo.mk 0)


main = do
    args <- getArgs
    let path = args !! 0

    r <- Parser.parseFile path $ Parser.moduleParser [TName "x"] Parser.defState
    --r <- Parser.parseFile path $ Parser.exprParser (patchedParserState $ ASTInfo.mk 0)
    case r of
        Left  e -> displayIO stdout $ Parser.renderErr e
        --Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (snd a))
        Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (view State.namespace $ snd a))