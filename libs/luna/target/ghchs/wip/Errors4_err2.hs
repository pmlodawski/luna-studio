{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

data X a b = X a
           | Y b


class TestC m1 m2 c | m1 m2 -> c where
    testC :: m1 a -> m2 a -> c


instance TestC (X a1) (X a2) Int where
    testC _ _ = 6

instance TestC (X a) (X a) Char where
    testC _ _ = 'a'



main = do
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Int Int)  -- result: 5
    print $ testC (Y 0 :: X Int Int) (Y 0 :: X Char Int) -- result: 6
