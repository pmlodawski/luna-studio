module Luna.Syntax.Builder.Cache (module Luna.Syntax.Builder.Cache, module X) where

import Luna.Syntax.Builder.Cache.Cache as X

--import Luna.Syntax.Builder.Cache.Lit   ()
--import Luna.Syntax.Builder.Cache.Val   ()
--import Luna.Syntax.Builder.Cache.Thunk ()
--import Luna.Syntax.Builder.Cache.Term  ()
--import Luna.Syntax.Builder.Cache.Draft ()

import Prologue

import Data.Variant.Patterns       (ANY, MatchSet)
--import Luna.Syntax.AST.Term
--import Luna.Syntax.Builder.Cache.Cache

--import qualified Luna.Syntax.AST.Layout as Layout
--import qualified Data.Variant.Patterns  as Rec
--import Data.Variant.Patterns (case', ANY(ANY))


----foo1 (x :: Val Layout.Static  v t) = case' x
----foo2 (x :: Val Layout.Dynamic v t) = case' x


--data IDT a = IDT a deriving (Show)



--foo1 :: Val Layout.Static Int IDT -> MatchSet (Val Layout.Static Int IDT) String -> String
--foo1 (x :: Val Layout.Static  Int IDT) = case' x
--{-# NOINLINE foo1 #-}

----foo2 :: (Rec.IsMatchSet '[Lit Int IDT] (Val Layout.Static Int IDT) String) => Val Layout.Static Int IDT -> matches -> String
----foo2 (x :: Val Layout.Static  Int IDT) = case' x
----foo2' (x :: Val Layout.Dynamic Int IDT) = case' x