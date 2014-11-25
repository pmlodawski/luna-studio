{-# LANGUAGE   GADTs #-} 
{-# LANGUAGE   DeriveDataTypeable #-} 
{-# LANGUAGE   ScopedTypeVariables #-} 



module Data.Type.Hide where

import Prelude hiding (lookup)
import qualified Data.HashMap.Lazy as M
import Data.HashMap.Lazy(HashMap)
import Unsafe.Coerce
import Data.Typeable
import System.Mem.Weak (Weak, mkWeak, deRefWeak)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)
import Control.Monad (liftM)

data HideType where
  HideType :: a -> HideType

unsafeFromHideType :: HideType -> a
unsafeFromHideType (HideType x) = unsafeCoerce x
