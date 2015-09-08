---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}


module Type.BaseType where

import Data.Typeable
import Data.Proxy.Utils
import Prelude

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

--baseOf :: BaseType (Proxy a) b => a -> b
--baseOf = baseType . toProxy

--------------------------------------------------------------------------------
-- Type classes
--------------------------------------------------------------------------------

--class BaseType a b | a -> b where
--    baseType :: a -> b

--class BaseType (a :: k) (b :: l) | a -> b, k -> l where
--    baseType :: Proxy a -> Proxy b

-- nie dziala, bug GHC? Brakuje jakby fundepow pomiedzy (j -> k) --> j oraz (j -> k) --> k
--instance           BaseType (a :: j -> k) (out :: l) => BaseType (a (t :: j) :: k) (out :: l)
--instance (out ~ a)                => BaseType (a :: k) (out :: k)


--instance {-# OVERLAPPABLE #-} BaseType (a :: kt -> kk) (out :: o) => BaseType (a (t :: kt) :: kk) out
--instance {-# OVERLAPPABLE #-}                    (out ~ (a :: k)) => BaseType (a :: k)            out


--instance {-# INCOHERENT #-} BaseType a out => BaseType (a t) out




--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- instance {-# OVERLAPPABLE #-} out~(Proxy b) => BaseType (Proxy b) out where
--     baseType _ = Proxy
--
-- instance {-# OVERLAPPABLE #-} ((BaseType (Proxy b) (Proxy x)), out~Proxy x) => BaseType (Proxy (b t)) out where
     --baseType _ = Proxy


--type family BaseTypeTF (t :: *) where
--    BaseTypeTF (Proxy ( (a :: k -> l) (b :: k) )) = BaseTypeTF (Proxy (a :: k -> l) )
--    BaseTypeTF (Proxy (a :: x) ) = Proxy (a :: x)

--class PEQ (a :: k) (b :: l) where
    -- === :: a -> b -> Bool


--class BaseType a b | a -> b where
--    baseType :: Proxy a -> Proxy b

--instance {-# OVERLAPPABLE #-} BaseType (t t1) t where
--    baseType _ = Proxy

--instance {-# OVERLAPPABLE #-} out ~ (a :: k) => BaseType (a :: k) out where baseType _ = Proxy
--instance {-# OVERLAPPABLE #-} BaseType (( (a :: l -> x) (t :: l)) ) (a :: l -> x) where baseType _ = Proxy

class BaseType a b | a -> b where
    baseType :: a -> b

instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy a) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4 t5)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4 t5 t6)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4 t5 t6 t7)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4 t5 t6 t7 t8)) out
instance {-# OVERLAPPABLE #-} out ~ (Proxy a) => BaseType (Proxy (a t1 t2 t3 t4 t5 t6 t7 t8 t9)) out



--test = do
--    --print $ (Proxy :: BaseTypeTF (Proxy (Maybe Int))) == (Proxy :: Proxy Int)
--    --print $ baseType (Proxy :: (Proxy (Either Int String))) == (Proxy :: Proxy (Maybe Int))
--    --print "yo"
--    return ()

-- TODO: czemu nie mozna zrefaktoryzowac tego wyzej? Wtedy nie dizala!
-- np. baseType $ toProxy $ Just (5::Int)

--data X a = X a deriving (Show)

--class Test a b | a -> b where
--    test :: a -> b

--instance Test (X a) Int where
--    test _ = 5

--main = do
--    print $ typeOf $ baseType (undefined :: Proxy (Maybe Bool))
--    print $ typeOf $ baseType (undefined :: Proxy (Either Bool String))
--    print $ typeOf $ baseOf (Just True)
    --print $ typeOf $ baseOf (Just return)

    --print $ test (X 1)


