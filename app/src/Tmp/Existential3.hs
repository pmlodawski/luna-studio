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

data FooLike = forall a. (Foo a, Show a) => FooLike a


instance Foo FooLike where
    foo (FooLike a) = foo a

instance Show FooLike where
    show (FooLike a) = show a

-- instance Show (Foo a) where
--     show a = "Foo"

-- newtype Dat = Dat [FooLike] deriving (Monoid)

-- data FooLike2 = FooLike2 (forall a. Foo a => a)


-- single :: (forall a. Foo a => a) -> Dat
-- single a = Dat [a]

-- append :: (forall a. Foo a => a) -> Dat -> Dat
-- prepend :: Foo a => a -> Dat -> Dat
-- prepend a dat = single a <> dat

-- append :: Foo a => a -> Dat -> Dat
-- append a dat = dat <> single a

-- infixr 9 -:
-- (-:) :: Foo a => a -> Dat -> Dat
-- (-:) = append

-- single :: Foo a => a -> Dat
-- single a = Dat [FooLike a]

-- a = Dat [FooLike (3 :: Int), FooLike 'a']

-- b = (3 :: Int) -: 'a' -: mempty


m = [FooLike (3 :: Int), FooLike 'a']
c = fmap foo m


main = do
    print "end"
