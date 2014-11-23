---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

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
import qualified Luna.Pass2.Analysis.Alias as AA
import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
import           Luna.Data.Namespace (Namespace(Namespace))

--patchedParserState info' = def
--    & ParserState.info .~ info'
--    & ParserState.conf .~ parserConf
--    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


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
        Right a -> do -- (putStrLn $ ppShow (fst a)) >> (putStrLn $ ppShow (view State.namespace $ snd a))
                   putStrLn $ ppShow $ (fst a) -- Parser.testme (fst a) Parser.defState
                   let ast = (fst a)
                   aa <- AA.run ast
                   case aa of
                       Left e  -> fail "error optaining aa"
                       Right a -> do
                           putStrLn $ ppShow $ a
                           s2 <- Stage2.run (Namespace [0] a) ast
                           putStrLn $ ppShow s2