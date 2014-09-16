module Test.Luna.Typechecker.AST.TIDSpec (spec) where

--import qualified Luna.Typechecker.AST.Alternatives as Alt
--import qualified Luna.Typechecker.AST.Common       as Cmm
--import qualified Luna.Typechecker.AST.Expr         as Exp
--import qualified Luna.Typechecker.AST.Kind         as Knd
--import qualified Luna.Typechecker.AST.Lit          as Lit
--import qualified Luna.Typechecker.AST.Module       as Mod
--import qualified Luna.Typechecker.AST.Pat          as Pat
--import qualified Luna.Typechecker.AST.Scheme       as Sch
--import qualified Luna.Typechecker.AST.Type         as Ty


--import qualified Luna.Typechecker.Ambiguity        as Amb
--import qualified Luna.Typechecker.Assumptions      as Ass
--import qualified Luna.Typechecker.BindingGroups    as Bnd
--import qualified Luna.Typechecker.ContextReduction as CxR
--import qualified Luna.Typechecker.HasKind          as HKd
--import qualified Luna.Typechecker.Substitutions    as Sub
--import qualified Luna.Typechecker.TIMonad          as TIM
--import qualified Luna.Typechecker.Typeclasses      as Tcl
--import qualified Luna.Typechecker.TypeInference    as Inf
--import qualified Luna.Typechecker.Unification      as Uni
--import qualified Luna.Typechecker                           as Typechecker



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
spec = do
  describe "type TID" $ do
    it "can be included inside of lazy State" $
      let res = Lz.evalState (do a <- lzGetTID
                                 b <- lzGetTID
                                 return [a /= b]) 0
       in res `shouldSatisfy` all id
    it "can be included inside of strict State" $
      let res = St.evalState (do a <- stGetTID
                                 b <- stGetTID
                                 return [a /= b]) 0
       in res `shouldSatisfy` all id
