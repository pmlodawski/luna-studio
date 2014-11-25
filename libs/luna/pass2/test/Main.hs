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
import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
import           Luna.Data.Namespace (Namespace(Namespace))
import qualified Luna.Pass as Pass
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as ByteStr
import Luna.Data.Source (Source(Source), Medium(File), Code(Code))
import qualified Luna.Data.Source as Source
--patchedParserState info' = def
--    & ParserState.info .~ info'
--    & ParserState.conf .~ parserConf
--    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


--parsePattern    pat  = Parser.parseString pat  $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0)
--parseExpression expr = Parser.parseString expr $ Parser.exprParser    (patchedParserState $ ASTInfo.mk 0)


main = do
    args <- getArgs
    let path = args !! 0
        src  = Source "Main" (File path)


    result <- runEitherT $ do
        (ast1, astinfo) <- Pass.run1_ Stage1.pass src
        aa1             <- Pass.run1_ AA.pass ast1
        ast2            <- Pass.run3_ Stage2.pass (Namespace [] aa1) astinfo ast1
        aa2             <- Pass.run1_ AA.pass ast2
        return (ast2,aa2)

    case result of
        Left e      -> putStrLn $ ppShow e
        Right (x,y) -> putStrLn (ppShow x) 
                    >> putStrLn (ppShow y)
    return ()
