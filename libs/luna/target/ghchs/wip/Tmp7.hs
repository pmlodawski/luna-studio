{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MagicHash #-}

!{-# LANGUAGE RightSideContexts #-}

import Data.Typeable
import GHC.Prim (unsafeCoerce#)

data V a = V a deriving (Show, Typeable)

class Test m out | m -> out where
    test :: m a -> out


instance Test V Int where
    test _ = 5

instance Test Proxy Int where
    test _ = 5



toProxy :: a -> Proxy a
toProxy _ = Proxy


data UV = UV deriving (Show, Typeable)
data UV' a = UV' a deriving (Show, Typeable)

class KnownVar a b | a -> b where
    markVars :: a -> b


--instance KnownVar Int out <= out~Int where
--    markVars = id

--instance KnownVar a out <= out~UV where
--    markVars _ = UV

instance KnownVar a (UV' a) where
    markVars = UV'

--instance KnownVar (V a1) out <= (out~(V a2), KnownVar a1 a2) where
--    markVars (V a) = V $ markVars a

tst :: Monad m => Proxy (V (m Int))
tst = Proxy

tst2 :: (forall a . a) -> Int
tst2 _ = 5

tst3 :: a -> Int
tst3 = unsafeCoerce#

main = do
    let x = markVars (return 5)

    --print $ tst3 (return 5)

    --let x = (unsafeCoerce# $ return 5 :: Int)

    --print $ test tst

    --print $ typeOf $ markVars ((return (5::Int)))

    --let x = test $ toProxy $ V (return 5)
    --print $ test $ toProxy $ V (return 5)
    print "end"
