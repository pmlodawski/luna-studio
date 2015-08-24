{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

class Foo a where
    foo :: a -> String


infixr 9 :-:
data DatList = forall a. Foo a => a :-: DatList
             | NoDat

instance Monoid DatList where
    mempty = NoDat
    d `mappend` d' = case d of
        NoDat    -> d'
        a :-: dd -> a :-: (dd `mappend` d')

-- single :: (forall a. Foo a => a) -> Dat
-- single a = Dat [a]

main = do
    print "end"

x = "a" :-: "B" :-: mempty


instance Foo String where
    foo = id
