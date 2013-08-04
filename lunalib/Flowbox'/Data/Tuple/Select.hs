--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple select 2
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Flowbox'.Data.Tuple.Select where
import Common'.C''select0
import Common'.C''select1
import Common'.C''select2
import Data.Tuple.OneTuple

instance C''select0 (OneTuple a) a where select0 (OneTuple x) = x

instance C''select0 (a0,a1) a0 where select0 (x,_) = x
instance C''select0 (a0,a1,a2) a0 where select0 (x,_,_) = x

instance C''select1 (a0,a1) a1 where select1 (_,x) = x
instance C''select1 (a0,a1,a2) a1 where select1 (_,x,_) = x

instance C''select2 (a0,a1,a2) a2 where select2 (_,_,x) = x

