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

class HideType a c where
    hideType   :: a -> c
    revealType :: c -> a

data Simple where
    Simple :: a -> Simple

instance Show Simple where
    show _ = "Hidden"

instance HideType a Simple where
    hideType              = Simple
    revealType (Simple x) = unsafeCoerce x


