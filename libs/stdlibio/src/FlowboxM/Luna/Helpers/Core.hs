{-# LANGUAGE MultiParamTypeClasses, 
             FunctionalDependencies, 
             KindSignatures, 
             DataKinds,
             FlexibleInstances,
             NoMonomorphismRestriction #-}

module FlowboxM.Luna.Helpers.Core (
    module Prelude,
	module FlowboxM.Luna.Helpers.Core,
    module FlowboxM.Luna.Helpers.TH.Inst,
    module FlowboxM.Luna.Helpers.StdLib,
    --module Flowbox.Luna.Libs.Std.Data.NTuple.Select,
    --module Flowbox.Luna.Libs.Std.Base
)
where

import           Prelude hiding((>>), (>>=), fail, return)
import qualified Prelude as Prelude
import           FlowboxM.Luna.Helpers.TH.Inst       
import           FlowboxM.Luna.Helpers.StdLib        
import Control.Applicative


--import           Flowbox.Luna.Libs.Std.Data.NTuple.Select   
--import           Flowbox.Luna.Libs.Std.Base     

import GHC.TypeLits            

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)


class Failure (a :: Symbol)

--class Get0 a b c | a -> b, a b -> c, a c -> b, a->c where
--    get0 :: a b -> c

--class Get1 a b c | a -> b, a b -> c, a c -> b, a->c where
--    get1 :: a b -> c

--class Get2 a b c | a b -> c, a c -> b where
--    get2 :: a b -> c


class Get0 m f |  m -> f where
    get0 :: m -> f

class Get1 m f |  m -> f where
    get1 :: m -> f

class Get2 m f |  m -> f where
    get2 :: m -> f

class Get3 m f |  m -> f where
    get3 :: m -> f


--instance Get0 (Pure Int) (Pure Int) where
--	get0 = id

instance Get0 (IO a) (IO a) where
    get0 = id

instance Get0 (Pure a) (Pure a) where
    get0 = id



--class Get0 a b | a -> b where
--    get0 :: a -> b

--class Get1 a b | a -> b where
--    get1 :: a -> b

--class Get2 a b | a -> b where
--    get2 :: a -> b


mkPure0  a = Pure $ a
mkPure1  a v1 = Pure $ a v1
mkPure2  a v1 v2 = Pure $ a v1 v2
mkPure3  a v1 v2 v3 = Pure $ a v1 v2 v3
mkPure4  a v1 v2 v3 v4 = Pure $ a v1 v2 v3 v4
mkPure5  a v1 v2 v3 v4 v5 = Pure $ a v1 v2 v3 v4 v5
mkPure6  a v1 v2 v3 v4 v5 v6 = Pure $ a v1 v2 v3 v4 v5 v6
mkPure7  a v1 v2 v3 v4 v5 v6 v7 = Pure $ a v1 v2 v3 v4 v5 v6 v7
mkPure8  a v1 v2 v3 v4 v5 v6 v7 v8 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8
mkPure9  a v1 v2 v3 v4 v5 v6 v7 v8 v9 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8 v9
mkPure10 a v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 = Pure $ a v1 v2 v3 v4 v5 v6 v7 v8 v9 v10



pureIO :: a -> IO a
pureIO = Prelude.return

failIO :: String -> IO a
failIO = Prelude.fail


tuple2 a b = (,) <$> a <*> b

(>>=) = bind
(>>)  = bind_
fail  = failIO
return a = a


defFunction0 f = f

defFunction1 f v1 = do
    v1' <- getIO v1
    f (Pure v1')

defFunction2 f v1 v2 = do
    v1' <- getIO v1
    v2' <- getIO v2
    f (Pure v1') (Pure v2')

defFunction3 f v1 v2 v3 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    f (Pure v1') (Pure v2') (Pure v3')

defFunction4 f v1 v2 v3 v4 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    v4' <- getIO v4
    f (Pure v1') (Pure v2') (Pure v3') (Pure v4')

defFunction5 f v1 v2 v3 v4 v5 = do
    v1' <- getIO v1
    v2' <- getIO v2
    v3' <- getIO v3
    v4' <- getIO v4
    v5' <- getIO v5
    f (Pure v1') (Pure v2') (Pure v3') (Pure v4') (Pure v5')