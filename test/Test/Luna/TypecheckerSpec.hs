module Test.Luna.TypecheckerSpec (spec) where

--import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Luna.Typechecker.Internal.AST.Common       as Cmm
--import qualified Luna.Typechecker.Internal.AST.Expr         as Exp
--import           Luna.Typechecker.Internal.AST.Expr         (Expr(..))
--import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
import           Luna.Typechecker.Internal.AST.Lit          (Lit(..))
--import qualified Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
--import qualified Luna.Typechecker.Internal.AST.Pat          as P (Pat(..))
import           Luna.Typechecker.Internal.AST.Pat          (Pat(..))
--import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
import           Luna.Typechecker.Internal.AST.Scheme       (Scheme(..),toScheme)
--import qualified Luna.Typechecker.Internal.AST.TID          as TID
--import           Luna.Typechecker.Internal.AST.TID          (TID)
import           Luna.Typechecker.Internal.AST.Type         (Type(..),fn,tInteger,list,tBool)
--import           Luna.Typechecker.Internal.AST.Type         (Type(..),Tyvar(..),fn,list)


--import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
import           Luna.Typechecker.Internal.Assumptions      (Assump(..))
--import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
import           Luna.Typechecker.Internal.BindingGroups    (Expr(..))
--import qualified Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Luna.Typechecker.Internal.TIMonad          as TIM
--import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import           Luna.Typechecker.Internal.Typeclasses      (Qual(..), initialEnv)
--import qualified Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Luna.Typechecker.Internal.Unification      as Uni
import           Luna.Typechecker                           (tiProgram)

import           Test.Hspec
--import           Control.Monad.IO.Class                     (MonadIO(..))
--import           Debug.Trace                                (trace)

--hspec_debug :: (Show a, MonadIO m) => a -> m ()
--hspec_debug = liftIO . putStrLn . show
--hspec_debug = return ()
--hspec_debug x = return $ trace (show x) ()

ap :: [Expr] -> Expr
ap = foldl1 Ap

spec :: Spec
spec = do
  describe "the typechecker interface for basic AST" $ do
    let const_type = Forall [Star, Star] ([] :=> (TGen 0 `fn` TGen 1 `fn` TGen 0))  
        const_pat  = [PVar "x", PVar "y"]
        const_body = Var "x"

        gcd_type  = Forall [] ([] :=> (tInteger `fn` tInteger `fn` tInteger))
        gcd_pat1  = [PVar "a", PLit (LitInt 0)]
        gcd_body1 = Var "a"
        gcd_pat2  = [PVar "a", PVar "b"]
        gcd_body2 = ap [Var "gcd", Var "b", ap [Var "mod", Var "a", Var "b"]]
        
        mod_type  = Forall [] ([] :=> (tInteger `fn` tInteger `fn` tInteger))

        cons_type = Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))
        nil_type  = Forall [Star] ([] :=> (list (TGen 0)))

        foldr_type  = Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
        foldr_pat1  = [PVar "f", PVar "a", PCon ("[]":>:nil_type) []]
        foldr_body1 = Var "a"
        foldr_pat2  = [PVar "f", PVar "a", PCon (":":>:cons_type) [PVar "x", PVar "xs"]]
        foldr_body2 = ap [Var "f", Var "x", ap [Var "foldr", Var "f", Var "a", Var "xs"]]

        and_type = Forall [] ([] :=> (list tBool `fn` tBool))
        and_pat  = []
        and_body = ap [Var "foldr", Var "(&&)", Const ("True" :>: toScheme tBool)]

    it "typechecks `const`" $ do
      let def = ([( "const", const_type , [( const_pat , const_body )] )] , [])
      tiProgram initialEnv [] [def] `shouldContain` ["const" :>: const_type]
    it "infers type for `const`" $ do
      let def = ([] , [[( "const", [(const_pat , const_body)] )]] )
      tiProgram initialEnv [] [def] `shouldContain` ["const" :>: const_type]
    it "typechecks `gcd`" $ do
      let def = ( [( "gcd", gcd_type , [ (gcd_pat1, gcd_body1)
                                       , (gcd_pat2, gcd_body2)
                                       ]
                  )] , [] )
      tiProgram initialEnv ["mod" :>: mod_type] [def] `shouldContain` ["gcd" :>: gcd_type]
    it "infers type for `gcd`" $ do
      let def = ( [] , [[( "gcd", [ (gcd_pat1, gcd_body1)
                                  , (gcd_pat2, gcd_body2)
                                  ]
                  )]] )
      tiProgram initialEnv ["mod" :>: mod_type] [def] `shouldContain` ["gcd" :>: gcd_type]
    it "typechecks `foldr`" $ do
      let def = ([("foldr", foldr_type, [(foldr_pat1, foldr_body1),(foldr_pat2,foldr_body2)])], [])
          res = tiProgram initialEnv [] [def]
      res `shouldContain` ["foldr" :>: foldr_type]
    it "infers type for `foldr`" $ do
      let def = ([], [[  ( "foldr", [ (foldr_pat1, foldr_body1)
                                    , (foldr_pat2, foldr_body2)])
                     ]]
                )
          res = tiProgram initialEnv [] [def]
      res `shouldContain` ["foldr" :>: foldr_type]
    it "typechecks `and`" $ do
      let def = ([("and", and_type, [(and_pat, and_body)])], [])
          res = tiProgram initialEnv ["foldr" :>: foldr_type, "(&&)" :>: toScheme (tBool `fn` tBool `fn` tBool)] [def]
      res `shouldContain` ["and" :>: and_type]
    it "infers type for `and`" $ do
      let def = ([], [[("and", [(and_pat, and_body)])]])
          res = tiProgram initialEnv ["foldr" :>: foldr_type, "(&&)" :>: toScheme (tBool `fn` tBool `fn` tBool)] [def]
      res `shouldContain` ["and" :>: and_type]
    it "typechecks `foldr` & infers `and`" $ do
      let defs = [ ([], [[  ( "foldr", [ (foldr_pat1, foldr_body1)
                                       , (foldr_pat2, foldr_body2)])
                        ]]
                   )
                 , ([], [[  ( "and", [(and_pat, and_body)])
                        ]]
                   )
                 ]
          res = tiProgram initialEnv ["(&&)" :>: toScheme (tBool `fn` tBool `fn` tBool)] defs
      res `shouldContain` ["foldr" :>: foldr_type]
      res `shouldContain` ["and" :>: and_type]
    it "has nice test coverage" $ do
      show foldr_type `shouldSatisfy` not.null
      show foldr_body1 `shouldSatisfy` not.null
      show foldr_body2 `shouldSatisfy` not.null
      show foldr_pat1 `shouldSatisfy` not.null
      show foldr_pat2 `shouldSatisfy` not.null