{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

data X a b = X a
           | Y b


class TestC m1 m2 where
    testC :: m1 a -> m2 a -> (TestF m1 m2) 


instance TestC (X a1) (X a2) where
    testC _ _ = 6

instance TestC (X a) (X a) where
    testC _ _ = 5


-- following type family works ONLY if BOTH equations result in Int!
type family TestF (a :: * -> * ) (b :: * -> * ) where
    TestF a a = Int
    TestF a b = Int


main = do
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Int Int)  -- result: 5
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Char Int) -- result: 6

--------------------------------------------------------
--but it DOES NOT if I want one of the instances result another type!
-- for example changing the above example to:


{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

data X a b = X a
           | Y b


class TestC m1 m2 where
    testC :: m1 a -> m2 a -> (TestF m1 m2) 


instance TestC (X a1) (X a2) where
    testC _ _ = 'a'

instance TestC (X a) (X a) where
    testC _ _ = 5


-- following type family works ONLY if BOTH equations result in Int!
type family TestF (a :: * -> * ) (b :: * -> * ) where
    TestF a a = Int
    TestF a b = Char


main = do
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Int Int)  -- result: 5
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Char Int) -- result: 6

--------------------
-- results in error:

    --Couldn't match expected type ‘TestF (X a1) (X a2)’
    --            with actual type ‘Char’
    --Relevant bindings include
    --  testC :: X a1 a -> X a2 a -> TestF (X a1) (X a2)
    --    (bound at /tmp/ghc1309_0/ghc1309_1.hspp:47:5)
    --In the expression: 'a'
    --In an equation for ‘testC’: testC _ _ = 'a'
    --In the instance declaration for ‘TestC (X a1) (X a2)’