{-
  The following is a prototype implementation of the plan for
  overloaded record fields in GHC, described at

  http://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/Plan

  This version does not support lens integration.
-}

{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses,
             TypeFamilies, RankNTypes, FlexibleInstances, 
             UndecidableInstances, PolyKinds, FlexibleContexts,
             NoMonomorphismRestriction, TypeOperators #-}

module TrivialRecords where

import Control.Applicative
import GHC.TypeLits


-- These class and type family declarations go in base:

type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t


-- Some example datatypes...

data Vector = Vector deriving Show

type instance GetResult Vector "foo" = forall a. (a -> a)


data V k = MkV { _foo'' :: Int, _bar'' :: k Int }
data X a = MkX { _foo''' :: Int, _bar''' :: a }


type instance GetResult (V k) "foo" = Int
instance t ~ Int => Has (V k) "foo" t where
  getField _ (MkV x _) = x

type instance GetResult (V k) "bar" = k Int
instance t ~ k Int => Has (V k) "bar" t where
  getField _ (MkV _ y) = y


type instance GetResult (X k) "foo" = Int
instance t ~ Int => Has (X k) "foo" t where
  getField _ (MkX x _) = x


type instance GetResult (X a) "bar" = a
instance t ~ a => Has (X a) "bar" t where
  getField _ (MkX _ y) = y



test = getField (Proxy :: Proxy "foo") . getField (Proxy :: Proxy "bar")


data Proxy k = Proxy

main = do
  print $ test $ MkX 5 (MkV 5 [4])
  print "end"