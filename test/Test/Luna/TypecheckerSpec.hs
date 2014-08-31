module Test.Luna.TypecheckerSpec (spec) where

--import qualified Luna.Typechecker.Internal.AST.Alternatives as Alt
--import qualified Luna.Typechecker.Internal.AST.Common       as Cmm
--import qualified Luna.Typechecker.Internal.AST.Expr         as Exp
import           Luna.Typechecker.Internal.AST.Expr         (Expr(..))
--import qualified Luna.Typechecker.Internal.AST.Kind         as Knd
import           Luna.Typechecker.Internal.AST.Kind         (Kind(..))
--import qualified Luna.Typechecker.Internal.AST.Lit          as Lit
--import qualified Luna.Typechecker.Internal.AST.Module       as Mod
--import qualified Luna.Typechecker.Internal.AST.Pat          as Pat
import qualified Luna.Typechecker.Internal.AST.Pat          as P (Pat(..))
--import           Luna.Typechecker.Internal.AST.Pat          (Pat(..))
--import qualified Luna.Typechecker.Internal.AST.Scheme       as Sch
import           Luna.Typechecker.Internal.AST.Scheme       (Scheme(..))
--import qualified Luna.Typechecker.Internal.AST.TID          as TID
--import           Luna.Typechecker.Internal.AST.TID          (TID)
--import qualified Luna.Typechecker.Internal.AST.Type         as Ty
import           Luna.Typechecker.Internal.AST.Type         (Type(..),Tyvar(..),fn,list)


--import qualified Luna.Typechecker.Internal.Ambiguity        as Amb
--import qualified Luna.Typechecker.Internal.Assumptions      as Ass
import qualified Luna.Typechecker.Internal.BindingGroups    as Bnd
--import qualified Luna.Typechecker.Internal.ContextReduction as CxR
--import qualified Luna.Typechecker.Internal.HasKind          as HKd
--import qualified Luna.Typechecker.Internal.Substitutions    as Sub
--import qualified Luna.Typechecker.Internal.TIMonad          as TIM
import qualified Luna.Typechecker.Internal.Typeclasses      as Tcl
import           Luna.Typechecker.Internal.Typeclasses      (Qual(..))
--import qualified Luna.Typechecker.Internal.TypeInference    as Inf
--import qualified Luna.Typechecker.Internal.Unification      as Uni
import qualified Luna.Typechecker                           as Typechecker

import           Test.Hspec
import           Control.Monad.IO.Class                     (MonadIO)
import           Debug.Trace                                (trace)

hspec_debug :: (Show a, MonadIO m) => a -> m ()
hspec_debug x = return $ trace (show x) ()
--hspec_debug = return ()
--hspec_debug = liftIO . putStrLn . show

spec :: Spec
spec = do
  describe "the typechecker interface for basic AST" $ do
    it "typechecks \"const\" with explicit typing" $ do
      let defs = [(
                    [
                      ( "const"
                        , Forall [] ([] :=> ((TVar (Tyvar "a" Star)) `fn` (TVar (Tyvar "b" Star)) `fn` (TVar (Tyvar "a" Star))))
                        , [ ([], NOP 0)
                          , ([P.Var 1 "x", P.Var 2 "y"], Var 3 "x")
                          , ([P.Var 4 "z", P.Wildcard 5], Var 6 "z")
                          ]
                      )
                    ],
                    []
                )]
          res = Typechecker.tiProgram Tcl.initialEnv [] defs
      hspec_debug res
      res `shouldSatisfy` not.null
    it "typechecks \"const\" with implicit typing" $ do
      let defs = [(
                    [],
                    [[
                      ( "const",
                        [ ([], NOP 0)
                        , ([P.Var 1 "x", P.Var 2 "y"], Var 3 "x")
                        , ([P.Var 4 "z", P.Wildcard 5], Var 6 "z")
                        ]
                      )
                    ]]
                 )]
          res = Typechecker.tiProgram Tcl.initialEnv [] defs
      hspec_debug res
      res `shouldSatisfy` not.null
    it "typechecks `foldr`" $ do
      let defs :: Bnd.BindGroup
          defs =  ( [
                      ( "foldr"
                      , Forall [Star, Star] ([] :=> ((TGen 0 `fn` TGen 1 `fn` TGen 0) `fn` TGen 1 `fn` list (TGen 0) `fn` TGen 1))
                      , [ ( [P.Var 0 "f", P.Var 1 "z", P.Con 2 "[]" (Forall [Star] ([] :=> (list (TGen 0)))) []]
                          , Var 3 "z"
                          )
                        , ( [P.Var 4 "f", P.Var 5 "z", P.Con 6 "(:)" (Forall [Star] ([] :=> (TGen 0 `fn` list (TGen 0) `fn` list (TGen 0)))) [P.Var 14 "x", P.Var 15 "xs"]]
                          , App 7 (Var 8 "f") [App 9 (Var 10 "foldr") [Var 11 "f", Var 12 "z", Var 13 "xs"]]
                          )
                        ]
                      )
                    ]
                  , [])
          res = Typechecker.tiProgram Tcl.initialEnv [] [defs]
      res `shouldSatisfy` not.null
    --[("foldr",
    --  Just (Forall [Star, Star]
    --         ([] :=>
    --          ((TGen 0 `fn` TGen 1 `fn` TGen 1) `fn` TGen 1 `fn` TAp tList (TGen 0) `fn` TGen 1))),
    --  [([PVar "f", PVar "z", PCon nilCfun []],
    --    evar "z"),
    --   ([PVar "f", PVar "z", PCon consCfun [PVar "x", PVar "xs"]],
    --    ap [evar "f", evar "x", ap [evar "foldr", evar "f", evar "z", evar "xs"]])])]
    --it "basic test: recursive function" $ do
    --  let defs :: Bnd.BindGroup
    --      defs = ([
    --                ( "headSafe"
    --                , Forall [] ([] :=> TVar (Tyvar "a" Star) `fn` tList (TVar (Tyvar "a" Star)) `fn` TVar (Tyvar "a" Star))
    --                , [ ([Var 0 "x", Var ])
    --                  ]
    --                )
    --              ],
    --              [])
    --      res = Typechecker.tiProgram Tcl.initialEnv [] [defs]
    --  res `shouldSatisfy` not.null
    --it "basic test: what happens when `foldr` and `and` are in the same binding group" $ do
    --  -- foldr f a (x:xs) = f x (foldr f a xs)
    --  -- foldr f a []     = a
    --  -- and xs           = foldr (&&) True xs
    --  let case01 = (App 8 (App 5 (Var 6 "f")
    --                                     [Var 7 "x"])
    --                          [App 14 (App 12 (App 9 (Var 10 "foldr")
    --                                                             [Var 11 "f"])
    --                                                  [Var 13 "a"])
    --                                      [Var 15 "xs"]])
    --      foldr_alts :: [Alt.Alt]
    --      foldr_alts = [
    --              ([Var 0 "f", Var 1 "a", Con 2 "(:)" (Sch.toScheme $ tCons $ TVar $ Tyvar "z" Star) [Var 3 "x", Var 4 "xs"]],
    --                case01),
    --              ([Wildcard 16, Var 17 "a", Wildcard 18],
    --                Var 19 "a")
    --            ]
    --      defs :: [Bnd.BindGroup]
    --      defs = [ ([], [[( "foldr", foldr_alts )]] ) ]
    --      res = Typechecker.tiProgram Tcl.initialEnv [] defs
    --  hspec_debug case01
    --  hspec_debug res
    --  res `shouldSatisfy` not.null
