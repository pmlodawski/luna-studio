{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}



module Data.Type.Hide where

import           Control.Monad     (liftM)
import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import           Data.Maybe        (fromJust)
import           Data.Typeable
import           Prelude           hiding (lookup)
import           System.IO.Unsafe  (unsafePerformIO)
import           System.Mem.Weak   (Weak, deRefWeak, mkWeak)
import           Unsafe.Coerce

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


