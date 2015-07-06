{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid

class Foo a where
    foo :: a -> String

instance Foo Char where
    foo a = "char"

instance Foo Int where
    foo a = "num"

data FooLike = forall a. Foo a => FooLike a

newtype Dat = Dat [FooLike] deriving (Monoid)

-- data FooLike2 = FooLike2 (forall a. Foo a => a)


-- single :: (forall a. Foo a => a) -> Dat
-- single a = Dat [a]

-- append :: (forall a. Foo a => a) -> Dat -> Dat
prepend :: Foo a => a -> Dat -> Dat
prepend a dat = single a <> dat

append :: Foo a => a -> Dat -> Dat
append a dat = dat <> single a

infixr 9 -:
(-:) :: Foo a => a -> Dat -> Dat
(-:) = append



single :: Foo a => a -> Dat
single a = Dat [FooLike a]

a = Dat [FooLike (3 :: Int), FooLike 'a']

b = (3 :: Int) -: 'a' -: mempty

main = do
    print "end"
