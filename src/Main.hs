{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- *------------------------------------------------
-- * Specification of the generic HM(X)
-- * type inference system in Haskell
-- *
-- * This instance deals with Ohori style records
-- *------------------------------------------------



module Main where


import qualified Luna.Parser.Parser as Parser

import qualified Luna.ASTNew.Decl       as Decl
import qualified Luna.ASTNew.Enum       as Enum
import qualified Luna.ASTNew.Label      as Label
import qualified Luna.ASTNew.Module     as Module
import qualified Luna.ASTNew.Name       as Name
import qualified Luna.ASTNew.Unit       as Unit



import           Luna.Data.Namespace               (Namespace (Namespace))
import           Luna.Data.Source                  (Code (Code), Medium (String), Source (Source))
import qualified Luna.Pass                         as Pass
import qualified Luna.Pass2.Analysis.Struct        as AA
import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
import           Text.Show.Pretty                  (ppShow)


import qualified Luna.ASTNew.Traversals       as AST
import           Luna.ASTNew.Decl             (LDecl)
import qualified Luna.Parser.State            as ParserState
import           Luna.Pass                    (PassMonad, PassCtx, Pass(Pass))
import           Luna.ASTNew.Enum             (Enumerated, IDTag(IDTag))
import           Luna.ASTNew.Expr             (LExpr, Expr)
import           Luna.ASTNew.Module           (Module(Module), LModule)
import           Data.Monoid                  (Monoid, mempty)
import           Control.Monad.State          (put, get, modify)
import           Luna.ASTNew.NameBase         (nameBase)
import qualified Luna.ASTNew.Pat              as Pat
import qualified Luna.ASTNew.Arg              as Arg
import Data.Text.Lazy (unpack)
import qualified Luna.Pass2.Analysis.Struct as SA
import qualified Luna.Pass2.Transform.Hash                 as Hash
import qualified Luna.Pass2.Target.HS.HASTGen              as HASTGen
import qualified Luna.Pass2.Target.HS.HSC                  as HSC
import qualified Luna.Pass2.Transform.SSA                  as SSA
import qualified Luna.Pass2.Transform.Desugar.ImplicitSelf as ImplSelf

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Either

import Control.Lens hiding (without)
import Data.List    (intercalate,intersperse)


-- TODO [kgdk]
import Inference as FooInfer
import Solver    as FooSolver







printer :: (Show a) => String -> a -> IO ()
printer x y = printer_aux x (show y)

printer_aux :: String -> String -> IO ()
printer_aux x y = do  f_print [Bold,White] "\n--------------------------------------------------------------------------------"
                      putStr "> "
                      f_print [Yellow] x
                      f_print [Bold,White] "--------------------------------------------------------------------------------\n"
                      putStrLn y

section :: IO () -> IO ()
section sec = do  sec
                  f_print [Bold, White] "\n\n################################################################################\n\n"


main :: IO ()
main = do f_print [Bold,Green] "MAIN"
          f_print [Cyan] "Reading `src/Maintest.luna`"
          maintest_luna <- do tmp <- readFile "src/Maintest.luna"
                              tmp `seq` return tmp
          f_print [Cyan] "Passes"
          let src = Source "Maintest_luna" (String maintest_luna)

          result <- runEitherT $ do
            (ast1, astinfo1) <- Pass.run1_ Stage1.pass src
            sa1              <- Pass.run1_ SA.pass ast1
            (ast2, astinfo2) <- Pass.run3_ Stage2.pass (Namespace [] sa1) astinfo1 ast1
            (ast3, astinfo3) <- Pass.run2_ ImplSelf.pass astinfo2 ast2
            constraints      <- Pass.run1_ FooInfer.tcpass ast3
            sa2              <- Pass.run1_ SA.pass ast3
            ast4             <- Pass.run1_ Hash.pass ast3
            ast5             <- Pass.run1_ SSA.pass ast4
            hast             <- Pass.run1_ HASTGen.pass ast5
            hsc              <- Pass.run1_ HSC.pass hast
            return  ( (ast1, astinfo1)
                    , sa1
                    , (ast2, astinfo2)
                    , (ast3, astinfo3)
                    , constraints
                    , sa2
                    , ast4
                    , ast5
                    , hast
                    , hsc
                    )

          case result of
            Left _                      -> f_print [Red, Bold] "some error, sorry"
            Right ( (ast1, astinfo1), sa1, (ast2, astinfo2), (ast3, astinfo3), constraints, sa2, ast4, ast5, hast, hsc ) -> do
              section $ do
                printer_aux " 1.1. ast1"        $ ppShow $ ast1
                printer_aux " 1.2. astinfo1"    $ ppShow $ astinfo1
                printer_aux " 2.   sa1"         $ ppShow $ sa1
                printer_aux " 3.1. ast2"        $ ppShow $ ast2
                printer_aux " 3.2. astinfo2"    $ ppShow $ astinfo2
                printer_aux " 4.1. ast3"        $ ppShow $ ast3
                printer_aux " 4.2. astinfo3"    $ ppShow $ astinfo3
                printer_aux " 5.   constraints" $ ppShow $ constraints
                printer_aux " 6.   sa2"         $ ppShow $ sa2
                printer_aux " 7.   ast4"        $ ppShow $ ast4
                printer_aux " 8.   ast5"        $ ppShow $ ast5
                printer_aux " 9.   hast"        $ ppShow $ hast
                printer_aux "10.   hsc"         $ unpack $ hsc



data PrintAttrs = Black
                | Red
                | Green
                | Yellow
                | Blue
                | Magenta
                | Cyan
                | White
                | Bold

attrtonum :: PrintAttrs -> Int
attrtonum Black   = 30
attrtonum Red     = 31
attrtonum Green   = 32
attrtonum Yellow  = 33
attrtonum Blue    = 34
attrtonum Magenta = 35
attrtonum Cyan    = 36
attrtonum White   = 37
attrtonum Bold    = 1

f_print :: [PrintAttrs] -> String -> IO ()
f_print fs x = do let fmt = intercalate ";" (map (show.attrtonum) fs)
                  putStr $ "\x1b[" ++ fmt ++ "m"
                  putStr x
                  putStrLn "\x1b[0m"




-- *------------------------------------------------
-- * Category: DATA DECLARATIONS
-- *------------------------------------------------





---- type inference

--tp :: (Typo, Term) -> TP Type
--tp (env, Id x) =  do a <- inst env x
--                     normalize a
----
--tp (env, Abs x e) = do a <- newtvar
--                       b <- tp (insert env (x, Mono (TV a)), e)
--                       normalize ((TV a) `Fun` b)

--tp (env, App e e') = do a <- newtvar
--                        t <- tp (env, e)
--                        t' <- tp (env, e')
--                        add_constraint (C [t `Subsume` (t' `Fun` TV a)])
--                        normalize (TV a)


--tp (env, Let x e e') = do a <- tp (env, e)
--                          b <- gen env a
--                          tp ((insert env (x, b)), e')

---- top-level program

--infer :: Term -> E (TVar, Subst, Constraint, Type)
--infer e = unTP (tp (init_typo, e)) (init_tvar, null_subst, true_cons)
----









