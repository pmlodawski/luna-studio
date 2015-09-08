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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DysfunctionalDependencies #-}

module TrivialRecords where

import Control.Applicative
import GHC.TypeLits
import TH


-- These class and type family declarations go in base:
data P k = P

type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t


class Property (name :: Symbol) base out | name base -> out where
  property :: P name -> base -> out

-- Some example datatypes...

data Vector = Vector deriving Show

instance (Property "foo" Vector (a -> out), Num out, Num a) => Property "foo2" Vector out where
  property _ base = property (P::P "foo") base 5 + 1

$(nop) 

instance Property "foo" Vector (a->Vector) where
  property _ _ = (\_ -> Vector)

$(nop) 

instance Property "bar" Vector (a->a) where
  property _ _ = id

$(nop) 





main = do
  print $ property (P::P "bar") (property (P::P "foo") Vector 5) 5
  print "end"