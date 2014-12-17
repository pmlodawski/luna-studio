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
import qualified Luna.Pass2.Analysis.Struct as SA
import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
import qualified Luna.Pass2.Transform.Desugar.ImplicitSelf as ImplSelf
import qualified Luna.Pass2.Transform.Hash                 as Hash
import qualified Luna.Pass2.Transform.SSA                  as SSA
import qualified Luna.Pass2.Target.HS.HASTGen              as HASTGen
import qualified Luna.Pass2.Target.HS.HSC                  as HSC
import           Luna.Data.Namespace (Namespace(Namespace))
import qualified Luna.Pass as Pass
import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as ByteStr
import Luna.Data.Source (Source(Source), Medium(File), Code(Code))
import qualified Luna.Data.Source as Source
import Data.Text.Lazy (unpack)



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
        (ast1, astinfo1) <- Pass.run1_ Stage1.pass src
        --return (ast2, astinfo2)
        sa1              <- Pass.run1_ SA.pass ast1
        (ast2, astinfo2) <- Pass.run3_ Stage2.pass (Namespace [] sa1) astinfo1 ast1
        (ast3, astinfo3) <- Pass.run2_ ImplSelf.pass astinfo2 ast2
        sa2              <- Pass.run1_ SA.pass ast3
        ast4             <- Pass.run1_ Hash.pass ast3
        ast5             <- Pass.run1_ SSA.pass ast4
        hast             <- Pass.run1_ HASTGen.pass ast5
        hsc              <- Pass.run1_ HSC.pass hast
        --return ast4
        --return (ast4, sa2)
        return ((ast5,sa2, hast), hsc)

    case result of
        Left  e -> putStrLn e
        Right (r,hsc) -> putStrLn (ppShow r)
                      *> putStrLn (unpack hsc)
    return ()
