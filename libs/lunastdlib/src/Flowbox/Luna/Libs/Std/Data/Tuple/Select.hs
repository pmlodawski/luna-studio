--snip-----------------
---- Machine generated code below, see Tools/MkTuple.hs
---- mkTuple select 5
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module Flowbox.Luna.Libs.Std.Data.Tuple.Select where
import           Flowbox.Luna.FClasses.U'select0   
import           Flowbox.Luna.FClasses.U'select1   
import           Flowbox.Luna.FClasses.U'select2   
import           Flowbox.Luna.FClasses.U'select3   
import           Flowbox.Luna.FClasses.U'select4   
import           Flowbox.Luna.FClasses.U'select5   
import           Data.Tuple.OneTuple               

instance C''select0' (OneTuple a) a where select0' (OneTuple x) = x; select0'''M = return . select0'

instance C''select0' (a0,a1) a0 where select0' (x,_) = x; select0'''M = return . select0'
instance C''select0' (a0,a1,a2) a0 where select0' (x,_,_) = x; select0'''M = return . select0'
instance C''select0' (a0,a1,a2,a3) a0 where select0' (x,_,_,_) = x; select0'''M = return . select0'
instance C''select0' (a0,a1,a2,a3,a4) a0 where select0' (x,_,_,_,_) = x; select0'''M = return . select0'
instance C''select0' (a0,a1,a2,a3,a4,a5) a0 where select0' (x,_,_,_,_,_) = x; select0'''M = return . select0'

instance C''select1' (a0,a1) a1 where select1' (_,x) = x; select1'''M = return . select1'
instance C''select1' (a0,a1,a2) a1 where select1' (_,x,_) = x; select1'''M = return . select1'
instance C''select1' (a0,a1,a2,a3) a1 where select1' (_,x,_,_) = x; select1'''M = return . select1'
instance C''select1' (a0,a1,a2,a3,a4) a1 where select1' (_,x,_,_,_) = x; select1'''M = return . select1'
instance C''select1' (a0,a1,a2,a3,a4,a5) a1 where select1' (_,x,_,_,_,_) = x; select1'''M = return . select1'

instance C''select2' (a0,a1,a2) a2 where select2' (_,_,x) = x; select2'''M = return . select2'
instance C''select2' (a0,a1,a2,a3) a2 where select2' (_,_,x,_) = x; select2'''M = return . select2'
instance C''select2' (a0,a1,a2,a3,a4) a2 where select2' (_,_,x,_,_) = x; select2'''M = return . select2'
instance C''select2' (a0,a1,a2,a3,a4,a5) a2 where select2' (_,_,x,_,_,_) = x; select2'''M = return . select2'

instance C''select3' (a0,a1,a2,a3) a3 where select3' (_,_,_,x) = x; select3'''M = return . select3'
instance C''select3' (a0,a1,a2,a3,a4) a3 where select3' (_,_,_,x,_) = x; select3'''M = return . select3'
instance C''select3' (a0,a1,a2,a3,a4,a5) a3 where select3' (_,_,_,x,_,_) = x; select3'''M = return . select3'

instance C''select4' (a0,a1,a2,a3,a4) a4 where select4' (_,_,_,_,x) = x; select4'''M = return . select4'
instance C''select4' (a0,a1,a2,a3,a4,a5) a4 where select4' (_,_,_,_,x,_) = x; select4'''M = return . select4'

instance C''select5' (a0,a1,a2,a3,a4,a5) a5 where select5' (_,_,_,_,_,x) = x; select5'''M = return . select5'

