module Test.Luna.Typechecker.AST.TIDSpec (spec) where



import           Luna.Typechecker.AST.TID

import           Test.Hspec

import qualified Control.Monad.State.Lazy                   as Lz
import qualified Control.Monad.State.Strict                 as St


lzGetTID :: Lz.State Int TID
lzGetTID = do
  x <- Lz.get
  Lz.put $ x + 1
  return $ enumTID x

stGetTID :: St.State Int TID
stGetTID = do
  x <- St.get
  St.put $ x + 1
  return $ enumTID x

spec :: Spec
spec =
  describe "type TID" $ do
    it "can be included inside of lazy State" $
      let res = Lz.evalState (do a <- lzGetTID
                                 b <- lzGetTID
                                 return [a /= b]) 0
       in res `shouldSatisfy` and
    it "can be included inside of strict State" $
      let res = St.evalState (do a <- stGetTID
                                 b <- stGetTID
                                 return [a /= b]) 0
       in res `shouldSatisfy` and
