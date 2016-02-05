{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DysfunctionalDependencies #-}
{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Target.HS.Data.Func.PolyArgs1 where

import           Data.Typeable
import           GHC.TypeLits

--data V = V deriving (Show)


--class GetMem o (name :: Symbol) (n :: Nat) f | o name -> f where
--    getMem :: o -> Proxy name -> Proxy n -> f


--instance (Foo a b, f~ (a->b)) => GetMem V "foo" n f where
--    getMem _ _ _ = foo


--class Foo a b | a -> b where
--    foo :: a -> b

--instance Foo [a] [a] where
--    foo = id




--instance Foo Int Int where
--    foo = (+1)


--test v = do
--    print $ getMem v (Proxy::Proxy "foo") (Proxy::Proxy 1) "ala"
--    print $ getMem v (Proxy::Proxy "foo") (Proxy::Proxy 2) (5::Int)
--    --print $ getMem v (Proxy::Proxy "foo") (Proxy::Proxy 3) []

--main = do
--    print $ foo "ala"
--    print $ foo (5::Int)
--    print $ foo []

--    print $ getMem V (Proxy::Proxy "foo") (Proxy::Proxy 1) "ala"
--    print $ getMem V (Proxy::Proxy "foo") (Proxy::Proxy 1) (5::Int)
--    print $ getMem V (Proxy::Proxy "foo") (Proxy::Proxy 1) []

--    return ()
