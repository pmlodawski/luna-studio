{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-} -- the flag is niot needed by the example

module Main where
import Data.Typeable


class CTest a b | a -> b where
    cTest :: a -> b
    cTest = undefined


-- this example is choosen even if more specific is available!
instance out~(m a) => CTest (m a) out where
    cTest = id

instance CTest [Int] String where
    cTest _ = "test"


main = do
    print $ typeOf $ cTest [5::Int]
