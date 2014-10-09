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


patchedParserState :: ASTInfo.ASTInfo
                   -> ParserState.State (Pragma Pragma.ImplicitSelf, 
                                        (Pragma Pragma.AllowOrphans, 
                                        (Pragma Pragma.TabLength, ())))
patchedParserState info' = def
    & ParserState.info .~ info'
    & ParserState.conf .~ parserConf
    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


parsePattern pat = Parser.parseString pat $ Parser.patternParser (patchedParserState $ ASTInfo.mk 0)




spec :: Spec
spec = do
    describe "Parser parse" $ do
        forM_ patterns (\pattern -> it ("pattern " ++ show pattern) $ do
            parsePattern pattern `shouldSatisfy` isRight)
