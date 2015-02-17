import           Data.Default
import           Flowbox.Prelude
import qualified Luna.DEP.Parser.Parser       as Parser
import qualified Luna.DEP.Parser.State        as State
import qualified Luna.DEP.Parser.State        as ParserState
import           System.Environment           (getArgs)
import           System.IO                    (stdout)
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty, (<>))
import           Text.Show.Pretty

import qualified Luna.DEP.Data.ASTInfo  as ASTInfo
import qualified Luna.DEP.Data.Config   as Config
import qualified Luna.DEP.Parser.Pragma as Pragma


patchedParserState info' = def
    & ParserState.info .~ info'
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


--parsePattern    pat  = Parser.parseString pat  $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0)
--parseExpression expr = Parser.parseString expr $ Parser.exprParser    (patchedParserState $ ASTInfo.mk 0)


main = do
    args <- getArgs
    let path = args !! 0

    r <- Parser.parseFile path $ Parser.moduleParser ["x"] Parser.defState
    --r <- Parser.parseFile path $ Parser.exprParser (patchedParserState $ ASTInfo.mk 0)
    case r of
        Left  e -> displayIO stdout $ Parser.renderErr e
        --Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (snd a))
        Right a -> (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (view State.namespace $ snd a))
