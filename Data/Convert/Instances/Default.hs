{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Convert.Instances.Default where

import Data.Convert.Base


instance {-# OVERLAPPABLE #-} Convertible a b => MaybeConvertible a b where
    tryConvert = return . convert

