{-# LANGUAGE KindSignatures, DataKinds, MultiParamTypeClasses,
             TypeFamilies, RankNTypes, FlexibleInstances, 
             UndecidableInstances, PolyKinds, FlexibleContexts,
             NoMonomorphismRestriction, TypeOperators #-}

module TrivialRecords where

import Control.Applicative
import GHC.TypeLits


type family GetResult (r :: *) (f :: Symbol) :: *


type instance GetResult Int "foo" = Int


class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t


instance (t~Int) => Has Int "foo" t where 
  getField _ _ = (5 :: Int)

-------------------------------

class Has2 r (f :: Symbol) t where
  getField2 :: proxy f -> r -> t

instance t ~ GetResult Int "foo" => Has2 Int "test" t where
  getField2 _ _ = (5 :: Int)


---- ERROR:
--    Could not deduce (t ~ Int)
--    from the context (t ~ GetResult Int "test")