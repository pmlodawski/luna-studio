{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.TypeLevel.Containers where

import           Prelude   (Bool (True, False))
import           Type.Bool hiding (Bool, False, True)


type family Elem a (cont :: p) :: Bool

type instance Elem a '[]       = 'False
type instance Elem a (t ': ts) = If (a == t) 'True (Elem a ts)


--class AppendEl el cont cont' | el cont -> cont' where
--    appendEl :: Proxy el -> Proxy cont -> Proxy cont'

--class AppendEl' (inplace :: Bool) el cont cont' | inplace el cont -> cont' where
--    appendEl' :: Proxy inplace -> Proxy el -> Proxy cont -> Proxy cont'


--instance (inplace ~ Elem el cont, AppendEl' inplace el cont cont') => AppendEl el cont cont' where
--    appendEl _ _ = appendEl' (Proxy :: Proxy inplace) (Proxy :: Proxy el) (Proxy :: Proxy cont)

--instance AppendEl' 'True el cont cont where
--    appendEl' _ _ _ = Proxy :: Proxy cont

--instance AppendEl' 'False el cont (el ': cont) where
--    appendEl' _ _ _ = Proxy :: Proxy (el ': cont)
