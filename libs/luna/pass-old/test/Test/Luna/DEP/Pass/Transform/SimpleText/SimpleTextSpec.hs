---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.DEP.Pass.Transform.SimpleText.SimpleTextSpec where

import           Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.DEP.Pass.Transform.SimpleText.Builder.Builder as Builder
import qualified Luna.DEP.Pass.Transform.SimpleText.Parser.Parser   as Parser
import qualified Test.Luna.DEP.AST.Common                           as Common
import qualified Test.Luna.DEP.Pass.Transform.Graph.Common          as Common
import qualified Test.Luna.DEP.Sample.Code                          as SampleCode


backAndForth :: String -> IO ()
backAndForth code = do
    targetExpr <- Common.getMain =<< (fst <$> Common.getAST code)
    emptyExpr  <- Common.getMain =<< (fst <$> Common.getAST SampleCode.emptyMain)
    --putStrLn code
    expr2 <- eitherToM' $ Parser.run code emptyExpr
    --prettyPrint expr2
    --prettyPrint pm2
    expr2 `shouldBe` targetExpr
    (code3, _) <- eitherToM' $ Builder.run def expr2
    code3 `shouldBe` code


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "text <-> ast conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth $ tail code) SampleCode.singleFun
