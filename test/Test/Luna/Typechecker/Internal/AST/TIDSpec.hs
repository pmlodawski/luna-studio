module Test.Luna.Typechecker.Internal.AST.TIDSpec (spec) where

import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
import qualified Luna.Typechecker.Internal.AST.Common       as Cmm
import qualified Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
import qualified Luna.Typechecker.Internal.AST.Module       as Mod
import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
import qualified Luna.Typechecker.Internal.AST.Type         as Ty


import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
import qualified Luna.Typechecker.Internal.ContextReduction as CxR
import qualified Luna.Typechecker.Internal.HasKind          as HKd
import qualified Luna.Typechecker.Internal.Substitutions    as Sub
import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import qualified Luna.Typechecker.Internal.TypeInference    as Inf
import qualified Luna.Typechecker.Internal.Unification      as Uni
import qualified Luna.Typechecker                           as Typechecker



import           Luna.Typechecker.Internal.AST.TID

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
