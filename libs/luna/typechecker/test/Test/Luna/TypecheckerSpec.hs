module Test.Luna.TypecheckerSpec (spec) where

--import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Luna.Typechecker.Internal.AST.Common       as Cmm
--import qualified Luna.Typechecker.Internal.AST.Expr         as Exp
import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
--import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
--import           Luna.Typechecker.Internal.AST.Pat          (Pat(..))
import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
--import qualified Luna.Typechecker.Internal.AST.TID          as TID
--import           Luna.Typechecker.Internal.AST.TID          (TID)
import qualified Luna.Typechecker.Internal.AST.Type         as Ty


--import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
--import qualified Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Luna.Typechecker.Internal.Unification      as Uni
import qualified Luna.Typechecker                           as Typechecker

import           Test.Hspec
import           Control.Monad.IO.Class (liftIO)

spec :: Spec
spec = do
  describe "Luna/Typechecker.hs" $ 
    it "does something" $ do
      let res = Typechecker.tiProgram Tcl.initialEnv [] preludeDefns
      liftIO $  putStrLn $ show res
      res `shouldSatisfy` not.null

preludeDefns :: [Bnd.BindGroup]
preludeDefns = [
    (
      [
      --["id" :>: Forall [] ([] :=> TAp (TAp (TCon (Tycon "(->)" * -> (* -> *))) (TVar (Tyvar "a" *))) (TAp (TAp (TCon (Tycon "(->)" * -> (* -> *))) (TVar (Tyvar "b" *))) (TVar (Tyvar "a" *))))]
        ( "id",
          Sch.Forall [] ([] Tcl.:=> ((Ty.TVar (Ty.Tyvar "a" Knd.Star)) `Ty.fn` (Ty.TVar (Ty.Tyvar "b" Knd.Star)) `Ty.fn` (Ty.TVar (Ty.Tyvar "a" Knd.Star)))),
          [

            --(
            --  [Pat.Var 0 "x"],
            --  (Exp.Var 0 "x")
            --)
          ]
        )
      ],
      []
    )
  ]
