{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DysfunctionalDependencies #-}

class CTest a b | a -> b where
    ctest :: a -> b

data X = X

instance Monad m => CTest X (m Int) where
    ctest _ = return 5

main = print (ctest X :: [Int])