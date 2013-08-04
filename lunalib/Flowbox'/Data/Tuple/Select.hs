--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple select 10
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Flowbox'.Data.Tuple.Select where
import Common'.C''select0
import Common'.C''select1
import Common'.C''select2
import Common'.C''select3
import Common'.C''select4
import Common'.C''select5
import Common'.C''select6
import Common'.C''select7
import Common'.C''select8
import Common'.C''select9
import Common'.C''select10
import Data.Tuple.OneTuple

instance C''select0 (OneTuple a) a where select0 (OneTuple x) = x; select0''M = return . select0

instance C''select0 (a0,a1) a0 where select0 (x,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2) a0 where select0 (x,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3) a0 where select0 (x,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4) a0 where select0 (x,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5) a0 where select0 (x,_,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5,a6) a0 where select0 (x,_,_,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5,a6,a7) a0 where select0 (x,_,_,_,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a0 where select0 (x,_,_,_,_,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a0 where select0 (x,_,_,_,_,_,_,_,_,_) = x; select0''M = return . select0
instance C''select0 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a0 where select0 (x,_,_,_,_,_,_,_,_,_,_) = x; select0''M = return . select0

instance C''select1 (a0,a1) a1 where select1 (_,x) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2) a1 where select1 (_,x,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3) a1 where select1 (_,x,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4) a1 where select1 (_,x,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5) a1 where select1 (_,x,_,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5,a6) a1 where select1 (_,x,_,_,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5,a6,a7) a1 where select1 (_,x,_,_,_,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a1 where select1 (_,x,_,_,_,_,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a1 where select1 (_,x,_,_,_,_,_,_,_,_) = x; select1''M = return . select1
instance C''select1 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a1 where select1 (_,x,_,_,_,_,_,_,_,_,_) = x; select1''M = return . select1

instance C''select2 (a0,a1,a2) a2 where select2 (_,_,x) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3) a2 where select2 (_,_,x,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4) a2 where select2 (_,_,x,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5) a2 where select2 (_,_,x,_,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5,a6) a2 where select2 (_,_,x,_,_,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5,a6,a7) a2 where select2 (_,_,x,_,_,_,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a2 where select2 (_,_,x,_,_,_,_,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a2 where select2 (_,_,x,_,_,_,_,_,_,_) = x; select2''M = return . select2
instance C''select2 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a2 where select2 (_,_,x,_,_,_,_,_,_,_,_) = x; select2''M = return . select2

instance C''select3 (a0,a1,a2,a3) a3 where select3 (_,_,_,x) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4) a3 where select3 (_,_,_,x,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5) a3 where select3 (_,_,_,x,_,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5,a6) a3 where select3 (_,_,_,x,_,_,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5,a6,a7) a3 where select3 (_,_,_,x,_,_,_,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a3 where select3 (_,_,_,x,_,_,_,_,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a3 where select3 (_,_,_,x,_,_,_,_,_,_) = x; select3''M = return . select3
instance C''select3 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a3 where select3 (_,_,_,x,_,_,_,_,_,_,_) = x; select3''M = return . select3

instance C''select4 (a0,a1,a2,a3,a4) a4 where select4 (_,_,_,_,x) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5) a4 where select4 (_,_,_,_,x,_) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5,a6) a4 where select4 (_,_,_,_,x,_,_) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5,a6,a7) a4 where select4 (_,_,_,_,x,_,_,_) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a4 where select4 (_,_,_,_,x,_,_,_,_) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a4 where select4 (_,_,_,_,x,_,_,_,_,_) = x; select4''M = return . select4
instance C''select4 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a4 where select4 (_,_,_,_,x,_,_,_,_,_,_) = x; select4''M = return . select4

instance C''select5 (a0,a1,a2,a3,a4,a5) a5 where select5 (_,_,_,_,_,x) = x; select5''M = return . select5
instance C''select5 (a0,a1,a2,a3,a4,a5,a6) a5 where select5 (_,_,_,_,_,x,_) = x; select5''M = return . select5
instance C''select5 (a0,a1,a2,a3,a4,a5,a6,a7) a5 where select5 (_,_,_,_,_,x,_,_) = x; select5''M = return . select5
instance C''select5 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a5 where select5 (_,_,_,_,_,x,_,_,_) = x; select5''M = return . select5
instance C''select5 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a5 where select5 (_,_,_,_,_,x,_,_,_,_) = x; select5''M = return . select5
instance C''select5 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a5 where select5 (_,_,_,_,_,x,_,_,_,_,_) = x; select5''M = return . select5

instance C''select6 (a0,a1,a2,a3,a4,a5,a6) a6 where select6 (_,_,_,_,_,_,x) = x; select6''M = return . select6
instance C''select6 (a0,a1,a2,a3,a4,a5,a6,a7) a6 where select6 (_,_,_,_,_,_,x,_) = x; select6''M = return . select6
instance C''select6 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a6 where select6 (_,_,_,_,_,_,x,_,_) = x; select6''M = return . select6
instance C''select6 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a6 where select6 (_,_,_,_,_,_,x,_,_,_) = x; select6''M = return . select6
instance C''select6 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a6 where select6 (_,_,_,_,_,_,x,_,_,_,_) = x; select6''M = return . select6

instance C''select7 (a0,a1,a2,a3,a4,a5,a6,a7) a7 where select7 (_,_,_,_,_,_,_,x) = x; select7''M = return . select7
instance C''select7 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a7 where select7 (_,_,_,_,_,_,_,x,_) = x; select7''M = return . select7
instance C''select7 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a7 where select7 (_,_,_,_,_,_,_,x,_,_) = x; select7''M = return . select7
instance C''select7 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a7 where select7 (_,_,_,_,_,_,_,x,_,_,_) = x; select7''M = return . select7

instance C''select8 (a0,a1,a2,a3,a4,a5,a6,a7,a8) a8 where select8 (_,_,_,_,_,_,_,_,x) = x; select8''M = return . select8
instance C''select8 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a8 where select8 (_,_,_,_,_,_,_,_,x,_) = x; select8''M = return . select8
instance C''select8 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a8 where select8 (_,_,_,_,_,_,_,_,x,_,_) = x; select8''M = return . select8

instance C''select9 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) a9 where select9 (_,_,_,_,_,_,_,_,_,x) = x; select9''M = return . select9
instance C''select9 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a9 where select9 (_,_,_,_,_,_,_,_,_,x,_) = x; select9''M = return . select9

instance C''select10 (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) a10 where select10 (_,_,_,_,_,_,_,_,_,_,x) = x; select10''M = return . select10

