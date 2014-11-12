---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Test.Luna.Pass.Transform.SimpleText.SimpleTextSpec where

import Test.Hspec

import           Flowbox.Control.Error
import           Flowbox.Prelude
import qualified Luna.Pass.Transform.SimpleText.Parser.Parser as Parser
import qualified Test.Luna.AST.Common                         as Common
import qualified Test.Luna.Pass.Transform.Graph.Common        as Common
import qualified Test.Luna.Sample.Code                        as SampleCode
import qualified Luna.Pass.Transform.SimpleText.Builder.Builder as Builder


backAndForth :: String -> IO ()
backAndForth code = do
    targetExpr <- Common.getMain =<< Common.getAST code
    emptyExpr  <- Common.getMain =<< Common.getAST SampleCode.emptyMain
    let pm = def
    --putStrLn code
    (expr2, pm2) <- eitherToM' $ Parser.run code pm emptyExpr
    --prettyPrint expr2
    --prettyPrint pm2
    expr2 `shouldBe` targetExpr
    (code3, _, _) <- eitherToM' $ Builder.run pm2 def expr2
    code3 `shouldBe` code
    return ()


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "text <-> ast conversion" $ do
        mapM_ (\(name, code) -> it ("returns the same when converting back and forth - " ++ name) $
                backAndForth $ tail code) SampleCode.singleFun
