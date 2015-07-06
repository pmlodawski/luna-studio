{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

import Data.Monoid

class Foo a where
    foo :: a -> String

instance Foo Char where
    foo a = "char"

instance Foo Int where
    foo a = "num"

data Dat = forall a. Foo a => Dat [a]

-- instance Monoid Dat where
--     mempty                    = Dat []
--     (Dat a) `mappend` (Dat b) = Dat (a <> b)

-- single :: (forall a. Foo a => a) -> Dat
-- single a = Dat [a]

a = Dat [3 :: Int, 'a' :: Char]

main = do
    print "end"
