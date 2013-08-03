--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple select 5
module Flowbox.Data.Tuple.Select where
import Data.Tuple.OneTuple

instance C'select0 (OneTuple a) a where select0 (OneTuple x) = x

import Common'.C'select0
instance C'select0 (a1,a2) a0 where select0 (x,_) = x
instance C'select0 (a1,a2,a3) a0 where select0 (x,_,_) = x
instance C'select0 (a1,a2,a3,a4) a0 where select0 (x,_,_,_) = x
instance C'select0 (a1,a2,a3,a4,a5) a0 where select0 (x,_,_,_,_) = x
instance C'select0 (a1,a2,a3,a4,a5,a6) a0 where select0 (x,_,_,_,_,_) = x

import Common'.C'select1
instance C'select1 (a1,a2) a1 where select1 (_,x) = x
instance C'select1 (a1,a2,a3) a1 where select1 (_,x,_) = x
instance C'select1 (a1,a2,a3,a4) a1 where select1 (_,x,_,_) = x
instance C'select1 (a1,a2,a3,a4,a5) a1 where select1 (_,x,_,_,_) = x
instance C'select1 (a1,a2,a3,a4,a5,a6) a1 where select1 (_,x,_,_,_,_) = x

import Common'.C'select2
instance C'select2 (a1,a2,a3) a2 where select2 (_,_,x) = x
instance C'select2 (a1,a2,a3,a4) a2 where select2 (_,_,x,_) = x
instance C'select2 (a1,a2,a3,a4,a5) a2 where select2 (_,_,x,_,_) = x
instance C'select2 (a1,a2,a3,a4,a5,a6) a2 where select2 (_,_,x,_,_,_) = x

import Common'.C'select3
instance C'select3 (a1,a2,a3,a4) a3 where select3 (_,_,_,x) = x
instance C'select3 (a1,a2,a3,a4,a5) a3 where select3 (_,_,_,x,_) = x
instance C'select3 (a1,a2,a3,a4,a5,a6) a3 where select3 (_,_,_,x,_,_) = x

import Common'.C'select4
instance C'select4 (a1,a2,a3,a4,a5) a4 where select4 (_,_,_,_,x) = x
instance C'select4 (a1,a2,a3,a4,a5,a6) a4 where select4 (_,_,_,_,x,_) = x

import Common'.C'select5
instance C'select5 (a1,a2,a3,a4,a5,a6) a5 where select5 (_,_,_,_,_,x) = x

