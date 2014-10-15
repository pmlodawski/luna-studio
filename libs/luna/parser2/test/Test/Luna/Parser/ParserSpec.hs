---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Parser.ParserSpec where

import Control.Monad (forM_)

import           Flowbox.Prelude
import           Flowbox.Test.QuickCheck
import qualified Luna.Data.ASTInfo       as ASTInfo
import qualified Luna.Data.Config        as Config
import qualified Luna.Parser.Parser      as Parser

--FIXME[wd]: following imports should be removed after moving to plugin based structure
--           including all use cases. Nothing should modify Parser.State explicitly!
import qualified Luna.Parser.Pragma as Pragma
import qualified Luna.Parser.State  as ParserState
import           Luna.Pragma.Pragma (Pragma)



main :: IO ()
main = hspec spec


patterns :: [String]
patterns = ["(a, b)", "_", "a"]


expressions :: [String]
expressions = [ "\"\"", "\"foo\"", "foo", "foo.bar", "foo.bar 1", "foo.bar.baz"
              , "foo.bar(baz, 12).boo", "foo.bar(baz, 12).boo", "1000e10"
              , "2 + 5", "10 * (25 + 5)", "Main", "Foo.Bar", "Main 5", "Foo.Bar(5).Baz"]


patchedParserState :: ASTInfo.ASTInfo
                   -> ParserState.State (Pragma Pragma.ImplicitSelf, 
                                        (Pragma Pragma.AllowOrphans, 
                                        (Pragma Pragma.TabLength, ())))
patchedParserState info' = def
    & ParserState.info .~ info'
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


parsePattern    pat  = Parser.parseString pat  $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0)
parseExpression expr = Parser.parseString expr $ Parser.exprParser    (patchedParserState $ ASTInfo.mk 0)

spec :: Spec
spec = do
    describe "Parser parse" $ do
        describe "Checking basic patterns parsing with AllowOrphans enabled" $ do
            forM_ patterns (\pattern -> it ("pattern " ++ show pattern) $ do
                parsePattern pattern `shouldSatisfy` isRight)
        describe "Checking basic expresssion parsing with AllowOrphans enabled" $ do
            forM_ expressions (\expr -> it ("expression " ++ show expr) $ do
                parseExpression expr `shouldSatisfy` isRight)
